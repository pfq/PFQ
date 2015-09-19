/***************************************************************
 *
 * (C) 2011 - Nicola Bonelli <nicola@pfq.io>
 *            Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 *
 ****************************************************************/

#include <iostream>
#include <fstream>
#include <sstream>

#include <thread>
#include <string>
#include <cstring>
#include <vector>
#include <algorithm>
#include <iterator>
#include <atomic>
#include <cmath>
#include <tuple>
#include <unordered_set>
#include <system_error>
#include <random>
#include <limits>


#include <pfq/pfq.hpp>
#include <pfq/util.hpp>

#include <linux/ip.h>
#include <linux/udp.h>
#include <linux/ethtool.h>
#include <linux/sockios.h>
#include <net/if.h>

#include <more/vt100.hpp>
#include <more/binding.hpp>
#include <more/affinity.hpp>


#ifdef HAVE_PCAP_H
#include <pcap/pcap.h>
#else
#warning "*** pfq-gen: compiling without pcap library ***"
#endif

using namespace pfq;


namespace more
{
    static std::unique_ptr<ethtool_cmd>
    ethtool_command(const char *ifname)
    {
        int sock = ::socket(AF_INET, SOCK_DGRAM, 0);
        if (sock == -1)
            throw std::system_error(errno, std::generic_category());

        std::unique_ptr<ethtool_cmd> ecmd(new ethtool_cmd);

        ecmd->cmd = ETHTOOL_GSET;

        struct ifreq ifreq_io;
        ifreq_io.ifr_data = reinterpret_cast<__caddr_t>(ecmd.get());
        strncpy(ifreq_io.ifr_name, ifname, IFNAMSIZ);

        if (ioctl(sock, SIOCETHTOOL, &ifreq_io) == -1) {
            ::close(sock);
            throw std::system_error(errno, std::generic_category());
        }

        ::close(sock);
        return std::move(ecmd);
    }
}

namespace opt
{
    size_t flush    = 1;
    size_t len      = 1514;
    size_t slots    = 4096;
    size_t npackets = std::numeric_limits<size_t>::max();
    size_t loop     = 1;
    size_t preload  = 1;
    int    copies   = 1;

    std::atomic_int nthreads;

    bool   rand_ip   = false;
    bool   rand_flow = false;
    bool   active_ts = false;
    bool   poisson   = false;

    double rate      = 0;

    std::vector< std::vector<int> > kthread;

    std::string file;
#ifdef HAVE_PCAP_H
    char errbuf[PCAP_ERRBUF_SIZE];
#endif
}


char *make_packets(size_t size, size_t numb)
{
    static unsigned char ping[98] =
    {
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xf0, 0xbf, /* L`..UF.. */
        0x97, 0xe2, 0xff, 0xae, 0x08, 0x00, 0x45, 0x00, /* ......E. */
        0x00, 0x54, 0xb3, 0xf9, 0x40, 0x00, 0x40, 0x01, /* .T..@.@. */
        0xf5, 0x32, 0xc0, 0xa8, 0x00, 0x02, 0xad, 0xc2, /* .2...... */
        0x23, 0x10, 0x08, 0x00, 0xf2, 0xea, 0x42, 0x04, /* #.....B. */
        0x00, 0x01, 0xfe, 0xeb, 0xfc, 0x52, 0x00, 0x00, /* .....R.. */
        0x00, 0x00, 0x06, 0xfe, 0x02, 0x00, 0x00, 0x00, /* ........ */
        0x00, 0x00, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, /* ........ */
        0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, /* ........ */
        0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, /* .. !"#$% */
        0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, /* &'()*+,- */
        0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, /* ./012345 */
        0x36, 0x37                                      /* 67 */
    };

    char * area = new char[size * numb];

    std::mt19937 gen;

    for(size_t i = 0; i < numb; ++i)
    {
        char * packet = area + i * size;

        memcpy(packet, ping, std::min(size, sizeof(ping)));

        for(auto n = sizeof(ping); n < size; n++)
        {
            packet[n] = static_cast<char>(0x38 + n - sizeof(ping));
        }

        /* randomize IP address */

        auto ip = reinterpret_cast<iphdr *>(packet + 14);
        if (opt::rand_ip)
        {
            ip->saddr = static_cast<uint32_t>(gen());
            ip->daddr = static_cast<uint32_t>(gen());
        }
    }

    return area;
}


using namespace more;

namespace thread
{
    struct context
    {
        context(int id, const thread_binding &b, std::vector<int> kcpu)
        : m_id(id)
        , m_bind(b)
        , m_pfq()
        , m_sent(std::unique_ptr<std::atomic_ullong>(new std::atomic_ullong(0)))
        , m_band(std::unique_ptr<std::atomic_ullong>(new std::atomic_ullong(0)))
        , m_gros(std::unique_ptr<std::atomic_ullong>(new std::atomic_ullong(0)))
        , m_fail(std::unique_ptr<std::atomic_ullong>(new std::atomic_ullong(0)))
        , m_gen()
        , m_packet(make_packets(opt::len, opt::preload))
        {
            if (m_bind.dev.empty())
                throw std::runtime_error("context[" + std::to_string (m_id) + "]: device unspecified");

            auto q = pfq::socket(param::list, param::tx_slots{opt::slots});

            std::cout << "thread     : " << id << " -> "  << show(m_bind) << " kthread { ";
            for(auto x : kcpu)
                std::cout << x  << ' ';
            std::cout << "}" << std::endl;

            for(unsigned int n = 0; n < m_bind.dev.front().queue.size(); n++)
            {
                q.bind_tx (m_bind.dev.front().name.c_str(), m_bind.dev.front().queue[n], n >= kcpu.size() ? -1 : kcpu.at(n));
            }

            q.enable();

            if (std::any_of(std::begin(kcpu), std::end(kcpu), [](int cpu) { return cpu != -1; }))
                    q.tx_async_start();

            m_pfq = std::move(q);
        }

        context(const context &) = delete;
        context& operator=(const context &) = delete;

        context(context &&) = default;
        context& operator=(context &&) = default;

        void operator()()
        {
            if (!m_packet)
                throw std::runtime_error("pool of packets empty!");

            if (!opt::file.empty()) {
#ifdef HAVE_PCAP_H
                pcap_generator();
#else
                throw std::runtime_error("pcap support disabled!");
#endif
            }
            else if (opt::poisson) {
                active_generator(true);
            }
            else if (opt::active_ts) {
                active_generator();
            }
            else if (opt::preload > 1) {
                pool_generator();
            }
            else generator();

            opt::nthreads--;
        }

        std::tuple<pfq_stats, uint64_t, uint64_t, uint64_t, uint64_t>
        stats() const
        {
            pfq_stats ret = {0,0,0,0,0,0,0};

            ret += m_pfq.stats();

            return std::make_tuple(ret, m_sent->load(std::memory_order_relaxed),
                                        m_band->load(std::memory_order_relaxed),
                                        m_gros->load(std::memory_order_relaxed),
                                        m_fail->load(std::memory_order_relaxed));
        }

    private:

        void generator()
        {
            std::cout << "generator  : online traffic started..." << std::endl;

            auto delta = std::chrono::nanoseconds(static_cast<uint64_t>(1000/opt::rate));
            auto ip    = reinterpret_cast<iphdr *>(m_packet.get() + 14);
            auto now   = std::chrono::system_clock::now();
            auto len   = opt::len;

            auto rc = opt::rate != 0.0;

            for(size_t n = 0; n < opt::npackets;)
            {
                if (rc)
                    rate_control(now, delta, n);

                if (!m_pfq.send_async(pfq::const_buffer(reinterpret_cast<const char *>(m_packet.get()), len), opt::flush, opt::copies))
                {
                    m_fail->fetch_add(1, std::memory_order_relaxed);
                    continue;
                }

                m_sent->fetch_add(1, std::memory_order_relaxed);
                m_band->fetch_add(len, std::memory_order_relaxed);
                m_gros->fetch_add(len+24, std::memory_order_relaxed);

                if (opt::rand_ip)
                {
                    ip->saddr = static_cast<uint32_t>(m_gen());
                    ip->daddr = static_cast<uint32_t>(m_gen());
                }

                n++;
            }
        }


        void pool_generator()
        {
            std::cout << "generator  : " << opt::preload << " preloaded packets..." << std::endl;

            auto delta = std::chrono::nanoseconds(static_cast<uint64_t>(1000/opt::rate));
            auto now   = std::chrono::system_clock::now();
            auto len   = opt::len;

            size_t idx = 0;

            auto rc = opt::rate != 0.0;

            for(size_t n = 0; n < opt::npackets;)
            {
                idx &= (opt::preload-1);

                if (rc)
                    rate_control(now, delta, n);

                if (!m_pfq.send_async(pfq::const_buffer(reinterpret_cast<const char *>(m_packet.get() + idx * opt::len), len), opt::flush, opt::copies))
                {
                    m_fail->fetch_add(1, std::memory_order_relaxed);
                    continue;
                }

                idx++;

                m_sent->fetch_add(1, std::memory_order_relaxed);
                m_band->fetch_add(len, std::memory_order_relaxed);
                m_gros->fetch_add(len+24, std::memory_order_relaxed);

                n++;
            }
        }


        void active_generator(bool poisson = false)
        {
            auto delta    = std::chrono::nanoseconds(static_cast<uint64_t>(1000/opt::rate));
            auto ip       = reinterpret_cast<iphdr *>(m_packet.get() + 14);
            auto now      = std::chrono::system_clock::now();
            auto len      = opt::len;

            std::chrono::nanoseconds pkt_time{0};

            try
            {
                if (poisson)
                {
                    auto link_speed_mbit = ethtool_cmd_speed(more::ethtool_command(m_bind.dev.front().name.c_str()).get());
                    pkt_time = std::chrono::nanoseconds( (len + 24)*8*1000/(link_speed_mbit) );
                    std::cout << "link       : " << link_speed_mbit/(1000.0) << " Gbit/sec" << std::endl;
                    std::cout << "pkt_tx_time: " << pkt_time.count() << " nsec" << std::endl;
                }
            }
            catch(...)
            {

                std::cout << "link       : could not detect speed of link!" << std::endl;
                std::cout << "pkt_tx_time: 0 nsec" << std::endl;
            }

            // poisson process traffic

            std::default_random_engine rnd;

            // we want to control the inter-packet gap instead of the departure time
            // otherwise we run into problems when the inter-departure time is less
            // than the packet length

            std::exponential_distribution<double> exp_dist(1.0 /(delta-pkt_time).count());

            for(size_t n = 0; n < opt::npackets;)
            {
                if (!m_pfq.send_at(pfq::const_buffer(reinterpret_cast<const char *>(m_packet.get()), len), now, opt::copies))
                {
                    m_fail->fetch_add(1, std::memory_order_relaxed);
                    continue;
                }

                if (poisson)
                    now += std::chrono::nanoseconds(static_cast<int>(exp_dist(rnd))) + pkt_time;
                else
                    now += delta;

                m_sent->fetch_add(1, std::memory_order_relaxed);
                m_band->fetch_add(len, std::memory_order_relaxed);
                m_gros->fetch_add(len+24, std::memory_order_relaxed);

                if (opt::rand_ip)
                {
                    ip->saddr = static_cast<uint32_t>(m_gen());
                    ip->daddr = static_cast<uint32_t>(m_gen());
                }

                n++;
            }
        }


#ifdef HAVE_PCAP_H
        void pcap_generator()
        {
            std::vector<uint32_t> rand_seed;
            struct pcap_pkthdr *hdr;
            u_char *data;

            auto rc = opt::rate != 0.0;

            if (opt::rand_flow)
            {
                for(int i = 0; i < 256; i++)
                    rand_seed.push_back(m_gen());
            }

            for (size_t l = 0; l < opt::loop; l++)
            {
                std::cout << "[PFQ] " << opt::file << " #" << (l+1) << "/" << opt::loop << "..." << std::endl;

                auto p = pcap_open_offline(opt::file.c_str(), opt::errbuf);
                if (p == nullptr)
                    throw std::runtime_error("pcap_open_offline:" + std::string(opt::errbuf));

                auto n = pcap_next_ex(p, &hdr, (u_char const **)&data);
                if (n == -2)
                    return;

                auto delta = std::chrono::nanoseconds(static_cast<uint64_t>(1000/opt::rate));

                auto now = std::chrono::system_clock::now();

                for(size_t i = 0; i < opt::npackets;)
                {
                    auto plen = std::min<size_t>(hdr->caplen, opt::len);

                    if (rc)
                        rate_control(now, delta, i);

                    if (auto ip = opt::rand_ip ? reinterpret_cast<iphdr *>(data + 14) : nullptr)
                    {
                        ip->saddr = static_cast<uint32_t>(m_gen());
                        ip->daddr = static_cast<uint32_t>(m_gen());
                    }

                    if (auto ip = opt::rand_flow ? reinterpret_cast<iphdr *>(data + 14) : nullptr)
                    {
                        auto hash = ip->saddr ^ ip->daddr;
                        auto seed = rand_seed[hash & 255];
                        ip->saddr ^= seed;
                        ip->daddr ^= seed;
                    }

                    if (!m_pfq.send_async(pfq::const_buffer(reinterpret_cast<const char *>(data), plen), opt::flush, opt::copies))
                    {
                        m_fail->fetch_add(1, std::memory_order_relaxed);
                        continue;
                    }

                    m_sent->fetch_add(1, std::memory_order_relaxed);
                    m_band->fetch_add(plen, std::memory_order_relaxed);
                    m_gros->fetch_add(plen+24, std::memory_order_relaxed);

                    n = pcap_next_ex(p, &hdr, (u_char const **)&data);
                    if (n == -2)
                        break;

                    i++;
                }

                pcap_close(p);
            }
        }
#endif

        template <typename Tp, typename Dur>
        void rate_control(Tp &now, Dur const &delta, int n)
        {
            if ((n & 8191) == 0)
            {
                while (std::chrono::system_clock::now() < (now + delta*8192))
                {}
                now = std::chrono::system_clock::now();
            }
        }

        int m_id;

        thread_binding m_bind;

        pfq::socket m_pfq;

        std::unique_ptr<std::atomic_ullong> m_sent;
        std::unique_ptr<std::atomic_ullong> m_band;
        std::unique_ptr<std::atomic_ullong> m_gros;
        std::unique_ptr<std::atomic_ullong> m_fail;

        std::mt19937 m_gen;

        std::unique_ptr<char[]> m_packet;
    };

}


bool any_strcmp(const char *arg, const char *opt)
{
    return strcmp(arg,opt) == 0;
}
template <typename ...Ts>
bool any_strcmp(const char *arg, const char *opt, Ts&&...args)
{
    return (strcmp(arg,opt) == 0 ? true : any_strcmp(arg, std::forward<Ts>(args)...));
}


template <typename U, typename T, typename Duration>
U persecond(T value, Duration dur)
{
    return static_cast<U>(value) * 1000000 /
        std::chrono::duration_cast<std::chrono::microseconds>(dur).count();
}

template <typename T>
std::string to_string_(std::ostringstream &out, T &&arg)
{
    out << std::move(arg);
    return out.str();
}
template <typename T, typename ...Ts>
std::string to_string_(std::ostringstream &out, T &&arg, Ts&&... args)
{
    out << std::move(arg);
    return to_string_(out, std::forward<Ts>(args)...);
}
template <typename ...Ts>
inline std::string
to_string(Ts&& ... args)
{
    std::ostringstream out;
    return to_string_(out, std::forward<Ts>(args)...);
}

template <typename T>
std::string
pretty(T value)
{
    if (value < 1000000000) {
    if (value < 1000000) {
    if (value < 1000) {
         return to_string(value);
    }
    else return to_string(value/1000, "_K");
    }
    else return to_string(value/1000000, "_M");
    }
    else return to_string(value/1000000000, "_G");
}


void usage(std::string name)
{
    throw std::runtime_error
    (
        "usage: " + std::move(name) + " [OPTIONS]\n\n"
        " -h --help                     Display this help\n"
        " -l --len INT                  Set packet length\n"
        " -n --packets INT              Number of packets\n"
        " -c --copies INT               Number of per-packet copies\n"
        " -s --queue-slots INT          Set Tx queue length\n"
        " -k --kthread IDX,IDX...       Async with kernel threads\n"
#ifdef HAVE_PCAP_H
        " -r --read FILE                Read pcap trace file to send\n"
        "    --loop                     Loop through the trace file N times\n"
#endif
        " -R --rand-ip                  Randomize IP addresses\n"
#ifdef HAVE_PCAP_H
        " -F --rand-flow                Randomize IP addresses (per-flow)\n"
#endif
        " -P --preload INT              Preload INT packets (must be a power of 2)\n"
        "    --rate DOUBLE              Packet rate in Mpps\n"
        " -a --active-tstamp            Use active timestamp as rate control\n"
        " -p --poisson                  Use a Poisson process for inter-packet gaps, implies -a\n"
        " -f --flush INT                Set flush length, used in sync Tx\n"
        " -t --thread BINDING\n\n"
        "      " + more::netdev_format + "\n" +
        "      " + more::thread_binding_format
    );
}


std::vector<thread::context *> thread_ctx;

int
main(int argc, char *argv[])
try
{
    if (argc < 2)
        usage(argv[0]);

    std::vector<more::thread_binding> binding;

    for(int i = 1; i < argc; ++i)
    {

#ifdef HAVE_PCAP_H
        if ( any_strcmp(argv[i], "-r", "--read") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("pcap file missing");
            }

            opt::file = argv[i];
            continue;
        }

        if ( any_strcmp(argv[i], "--loop") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("number missing");
            }

            opt::loop = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if ( any_strcmp(argv[i], "-F", "--rand-flow") )
        {
            opt::rand_flow = true;
            continue;
        }

#endif

        if ( any_strcmp(argv[i], "-f", "--flush") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("hint missing");
            }

            opt::flush = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if ( any_strcmp(argv[i], "-c", "--copies") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("hint missing");
            }

            opt::copies = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if ( any_strcmp(argv[i], "-l", "--len") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("length missing");
            }

            opt::len = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if ( any_strcmp(argv[i], "-n", "--packets") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("number missing");
            }

            opt::npackets = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if ( any_strcmp(argv[i], "-P", "--preload") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("number missing");
            }

            opt::preload = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if ( any_strcmp(argv[i], "-s", "--slots") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("slots missing");
            }

            opt::slots = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if ( any_strcmp(argv[i], "-k", "--kthread") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("kthread list missing");
            }

            auto vec = pfq::split(argv[i], ",");
            auto cores = pfq::fmap([](const std::string &val) -> int { return std::stoi(val); }, vec);

            opt::kthread.push_back(std::move(cores));
            continue;
        }

        if ( any_strcmp(argv[i], "-R", "--rand-ip") )
        {
            opt::rand_ip = true;
            continue;
        }

        if ( any_strcmp(argv[i], "-a", "--active-tstamp") )
        {
            opt::active_ts = true;
            continue;
        }

        if ( any_strcmp(argv[i], "-p", "--poisson") )
        {
            opt::poisson = true;
            opt::active_ts = true;
            continue;
        }

        if ( any_strcmp(argv[i], "-t", "--thread") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("binding missing");
            }

            binding.push_back(read_thread_binding(argv[i]));
            continue;
        }

        if ( any_strcmp(argv[i], "--rate") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("rate missing");
            }

            opt::rate = atof(argv[i]);
            continue;
        }

        if ( any_strcmp(argv[i], "-?", "-h", "--help") )
            usage(argv[0]);

        throw std::runtime_error(std::string("pfq-gen: ") + argv[i] + " unknown option");
    }

    //
    // process kthread:
    //

    while (opt::kthread.size() < binding.size())
        opt::kthread.push_back(std::vector<int>{});

    //
    // process binding:
    //

    size_t queue_num = 0;

    for(size_t i = 0; i < binding.size(); ++i)
    {
        if (binding[i].dev.empty())
            throw std::runtime_error("binding error: device unspecified");

        std::cout << "device     : " << show(binding[i].dev.front()) << std::endl;

        while (binding.at(i).dev.front().queue.size() < opt::kthread.at(i).size())
            binding.at(i).dev.front().queue.push_back(queue_num++);

        while (opt::kthread.at(i).size() < binding.at(i).dev.front().queue.size())
            opt::kthread.at(i).push_back(-1);

        if (binding.at(i).dev.front().queue.empty())
        {
            binding.at(i).dev.front().queue.push_back(-1);
            opt::kthread.at(i).push_back(-1);
        }
    }

    std::cout << "rand_ip    : "  << std::boolalpha << opt::rand_ip << std::endl;
    std::cout << "len        : "  << opt::len << std::endl;
    std::cout << "flush-hint : "  << opt::flush << std::endl;

    if (opt::npackets != std::numeric_limits<size_t>::max())
        std::cout << "npackets   : " << opt::npackets << std::endl;

    std::cout << "copies     : "  << opt::copies << std::endl;

    if (opt::rate != 0.0)
        std::cout << "rate       : "  << opt::rate << " Mpps" << std::endl;

    if (opt::slots == 0)
        throw std::runtime_error("tx_slots set to 0!");

    if (opt::rand_flow && opt::file.empty())
        throw std::runtime_error("random flow requires reading packets from file (r)");

    if (opt::active_ts && !opt::poisson)
        std::cout << "timestamp  : active!" << std::endl;

    if (opt::poisson)
        std::cout << "timestamp  : active with poisson Process traffic!" << std::endl;

    auto mq = std::any_of(std::begin(binding), std::end(binding), [](more::thread_binding const &b) { return b.dev.front().queue.size() > 1; });

    if (!opt::rand_ip && mq)
    {
        std::cout << vt100::BOLD << "*** Multiple queue detected! Consider to randomize IP addresses with -R option! ***" << vt100::RESET << std::endl;
    }

    // HugePages warning...
    //

    if (pfq::hugepages_mountpoint().empty())
        std::cout << "*** Warning: HugePages not mounted ***" << std::endl;

    //
    // preloaded packets must be a power of 2
    //

    if (opt::preload & (opt::preload-1))
        throw std::runtime_error("preloaded packets must be a power of 2");

    //
    // create thread context:
    //

    for(unsigned int i = 0; i < binding.size(); ++i)
    {
        thread_ctx.push_back(new thread::context(static_cast<int>(i), binding[i], opt::kthread.at(i)));
    }

    opt::nthreads.store(binding.size(), std::memory_order_relaxed);

    //
    // create threads:
    //

    size_t i = 0;
    std::for_each(binding.begin(), binding.end(), [&](more::thread_binding const &b) {

        auto t = new std::thread(std::ref(*thread_ctx[i++]));

        more::set_affinity(*t, b.cpu);

        t->detach();
    });

    pfq_stats cur, prec = {0,0,0,0,0,0,0};

    uint64_t sent, sent_ = 0;
    uint64_t band, band_ = 0;
    uint64_t gros, gros_ = 0;
    uint64_t fail, fail_ = 0;

    std::cout << "------------ gen started ------------\n";

    auto begin = std::chrono::system_clock::now();

    for(int y=0; true; y++)
    {
        std::this_thread::sleep_for(std::chrono::seconds(1));

        cur = {0,0,0,0,0,0,0};
        sent = 0;
        band = 0;
        gros = 0;
        fail = 0;

        std::for_each(thread_ctx.begin(), thread_ctx.end(), [&](const thread::context *c)
        {
            auto p = c->stats();

            cur  += std::get<0>(p);
            sent += std::get<1>(p);
            band += std::get<2>(p);
            gros += std::get<3>(p);
            fail += std::get<4>(p);
        });

        auto end   = std::chrono::system_clock::now();
        auto delta = end-begin;

        std::cout << "stats: { " << cur << " } -> app: { "
                  << vt100::BOLD
                  << "sent: " << persecond<int64_t>(sent - sent_, delta)            << ' '
                  << "fail: " << persecond<int64_t>(fail-fail_, delta)              << ' '
                  << "band: " << pretty(persecond<double>((band-band_)*8, delta))  << "bit/sec "
                  << "gros: " << pretty(persecond<double>((gros-gros_)*8, delta))  << "bit/sec "
                  << vt100::RESET << " } - "
                  << "socket: { "
                  << "sent: " << vt100::BOLD << persecond<int64_t>(cur.sent - prec.sent, delta) << vt100::RESET << " pkt/sec - "
                  << "disc: " << vt100::BOLD << persecond<int64_t>(cur.disc - prec.disc, delta) << vt100::RESET << " pkt/sec "
                  << " }" << std::endl;


        prec = cur, begin = end;
        sent_ = sent;
        band_ = band;
        gros_ = gros;
        fail_ = fail;

        if (opt::nthreads.load(std::memory_order_relaxed) == 0)
            break;
    }

    std::cout << "Shutting down sockets in 1 sec..." << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(1));

    cur  = {0,0,0,0,0,0,0};
    sent = 0;

    std::for_each(thread_ctx.begin(), thread_ctx.end(), [&](const thread::context *c)
    {
        auto p = c->stats();

        cur  += std::get<0>(p);
        sent += std::get<1>(p);
    });

    std::cout << "Summary:" << std::endl;

    std::cout << "    PFQ packets sent:" << cur.sent << " discarded:" << cur.disc << std::endl;
    std::cout << "    App packets sent:" << sent << std::endl;

}
catch(std::exception &e)
{
    std::cerr << e.what() << std::endl;
}
