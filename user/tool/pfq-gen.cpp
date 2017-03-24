/***************************************************************
 *
 * (C) 2011 - Nicola Bonelli <nicola@pfq.io>
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
#include <csignal>

#include <pfq/pfq.hpp>
#include <pfq/util.hpp>

#include <linux/ip.h>
#include <linux/udp.h>
#include <linux/ethtool.h>
#include <linux/sockios.h>
#include <net/if.h>
#include <netinet/ether.h>
#include <arpa/inet.h>

#include <more/vt100.hpp>
#include <more/binding.hpp>
#include <more/affinity.hpp>
#include <more/pretty.hpp>


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
        return ecmd;
    }
}


namespace opt
{
    size_t queue_sync   = 1;
    size_t len          = 1514;
    size_t slots        = 8192;
    size_t npackets     = std::numeric_limits<size_t>::max();
    size_t seconds      = std::numeric_limits<size_t>::max();
    size_t loop         = 1;
    size_t preload      = 1;
    unsigned int copies = 1;

    std::atomic_int nthreads;
    std::atomic_bool stop;

    bool   rand_src_ip = false;
    bool   rand_dst_ip = false;

    bool   rand_flow   = false;
    bool   active_ts   = false;
    bool   poisson     = false;
    bool   interactive = false;
    double rate = 0;

    uint32_t src_ip;
    uint32_t dst_ip;

    uint16_t src_port;
    uint16_t dst_port;

    uint32_t rand_depth = 32;
    uint32_t rand_flow_depth = 8;

    std::vector<uint32_t> rand_seed;
    std::vector< std::vector<int> > kthread;

    std::string file;

    std::string src_mac;
    std::string dst_mac;

#ifdef HAVE_PCAP_H
    char errbuf[PCAP_ERRBUF_SIZE];
#endif
}


char *make_packets( size_t size
                  , uint32_t src_ip
                  , uint32_t dst_ip
                  , uint16_t src_port
                  , uint16_t dst_port
                  , size_t numb)
{
    static unsigned char payload[34] =
    {
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xf0, 0xbf, /* L`..UF.. */
        0x97, 0xe2, 0xff, 0xae, 0x08, 0x00, 0x45, 0x00, /* ......E. */
        0x00, 0x54, 0xb3, 0xf9, 0x40, 0x00, 0x40, 0x11, /* .T..@.@. */
        0xf5, 0x32, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* .2...... */
        0x00, 0x00
    };

    if (!opt::dst_mac.empty())
    {
        struct ether_addr addr;
        if (ether_aton_r(opt::dst_mac.c_str(), &addr) == nullptr)
            throw std::runtime_error("ether_addr: " + opt::dst_mac + " bad mac address!");
        memcpy(payload, addr.ether_addr_octet, 6);
    }

    if (!opt::src_mac.empty())
    {
        struct ether_addr addr;
        if (ether_aton_r(opt::src_mac.c_str(), &addr) == nullptr)
            throw std::runtime_error("ether_addr: " + opt::src_mac + " bad mac address!");
        memcpy(payload+6, addr.ether_addr_octet, 6);
    }

    char * area = new char[size * numb];

    std::mt19937 gen;

    uint32_t rand_mask = ((1U << opt::rand_depth)-1);

    for(size_t i = 0; i < numb; ++i)
    {
        char * packet = area + i * size;

        memcpy(packet, payload, std::min(size, sizeof(payload)));

        for(auto n = sizeof(payload); n < size; n++)
        {
            packet[n] = static_cast<char>(0x38 + n - sizeof(payload));
        }

        auto ip = reinterpret_cast<iphdr *>(packet + 14);

        /* IP len */

        ip->tot_len = htons(size - 14);

        /* IP addresses */

        ip->saddr = src_ip;
        ip->daddr = dst_ip;

        /* randomize IP address */

        if (opt::rand_src_ip)
        {
            ip->saddr = src_ip | htonl(static_cast<uint32_t>(gen()) & rand_mask);
        }
        if (opt::rand_dst_ip)
        {
            ip->daddr = dst_ip | htonl(static_cast<uint32_t>(gen()) & rand_mask);
        }


        /* UDP header */

        auto udp = reinterpret_cast<udphdr *>(packet + 34);
        udp->len = htons(size - 34);
        udp->source = htons(src_port);
        udp->dest   = htons(dst_port);
    }

    return area;
}


using namespace more;

namespace thread
{
    struct context
    {
        context(int id, const thread_binding &b, std::vector<int> kthread)
        : m_id(id)
        , m_bind(b)
        , m_pfq()
        , m_sent(std::unique_ptr<std::atomic_ullong>(new std::atomic_ullong(0)))
        , m_band(std::unique_ptr<std::atomic_ullong>(new std::atomic_ullong(0)))
        , m_gros(std::unique_ptr<std::atomic_ullong>(new std::atomic_ullong(0)))
        , m_fail(std::unique_ptr<std::atomic_ullong>(new std::atomic_ullong(0)))
        , m_gen()
        , m_packet(make_packets(opt::len, opt::src_ip, opt::dst_ip, opt::src_port, opt::dst_port, opt::preload))
        {
            if (m_bind.dev.empty())
                throw std::runtime_error("context[" + std::to_string (m_id) + "]: device unspecified");

            m_async = kthread != std::vector<int>{-1};

            auto q = pfq::socket(param::list, param::tx_slots{opt::slots});

            std::cout << "thread     : " << id << " -> "  << show(m_bind) << " kthread { ";
            for(auto x : kthread)
                std::cout << x  << ' ';
            std::cout << "}" << std::endl;

            for(unsigned int n = 0; n < m_bind.dev.front().queue.size(); n++)
            {
                std::cout << "tx_bind    : " << m_bind.dev.front().name << ':' << m_bind.dev.front().queue[n];
                if (kthread.at(n) >= 0)
                    std::cout << " -> [kpfq/" <<  kthread.at(n) << "]" << std::endl;
                else
                    std::cout << std::endl;

                q.bind_tx (m_bind.dev.front().name.c_str(), m_bind.dev.front().queue[n], kthread.at(n));
            }

            m_pfq = std::move(q);
        }

        context(const context &) = delete;
        context& operator=(const context &) = delete;

        context(context &&) = default;
        context& operator=(context &&) = default;

        void operator()()
        {
            m_pfq.enable();

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
                active_generator(false);
            }
            else if (opt::preload > 1) {
                pool_generator();
            }
            else generator();

            opt::nthreads--;

            m_pfq.disable();
        }

        std::tuple<pfq_stats, uint64_t, uint64_t, uint64_t, uint64_t>
        stats() const
        {
            pfq_stats ret = {0,0,0,0,0,0,0,0};

            ret += m_pfq.stats();

            return std::make_tuple(ret, m_sent->load(std::memory_order_relaxed),
                                        m_band->load(std::memory_order_relaxed),
                                        m_gros->load(std::memory_order_relaxed),
                                        m_fail->load(std::memory_order_relaxed));
        }

    private:

        void wait_keyboard()
        {
            std::cout << "# " << m_sent->load(std::memory_order_relaxed) << ": to transmit the next packet press ENTER..." << std::flush;
            getchar();
        }

        void generator()
        {
            std::cout << "generator  : online traffic started..." << std::endl;

            auto delta = std::chrono::nanoseconds(static_cast<uint64_t>(1000/opt::rate));
            auto ip    = reinterpret_cast<iphdr *>(m_packet.get() + 14);
            auto now   = std::chrono::system_clock::now();
            auto len   = opt::len;

            auto rc = opt::rate != 0.0;

            uint32_t rand_mask = ((1U << opt::rand_depth)-1);

            for(size_t n = 0; n < opt::npackets;)
            {
                if (rc)
                    rate_control(now, delta, n);

                if (opt::interactive)
                    wait_keyboard();

                if (m_async)
                {
                    if (!m_pfq.send_async(pfq::const_buffer(reinterpret_cast<const char *>(m_packet.get()), len), opt::copies))
                    {
                        m_fail->fetch_add(1, std::memory_order_relaxed);
                        continue;
                    }
                }
                else
                {
                    if (!m_pfq.send(pfq::const_buffer(reinterpret_cast<const char *>(m_packet.get()), len), opt::queue_sync, opt::copies))
                    {
                        m_fail->fetch_add(1, std::memory_order_relaxed);
                        continue;
                    }
                }

                m_sent->fetch_add(1, std::memory_order_relaxed);
                m_band->fetch_add(len, std::memory_order_relaxed);
                m_gros->fetch_add(len+24, std::memory_order_relaxed);

                if (opt::rand_src_ip)
                {
                    ip->saddr = opt::src_ip | htonl(static_cast<uint32_t>(m_gen()) & rand_mask);
                }
                if (opt::rand_dst_ip)
                {
                    ip->daddr = opt::dst_ip | htonl(static_cast<uint32_t>(m_gen()) & rand_mask);
                }

                n++;

                if (opt::stop.load(std::memory_order_relaxed))
                    break;
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

                if (opt::interactive)
                    wait_keyboard();

                if (m_async)
                {
                    if (!m_pfq.send_async(pfq::const_buffer(reinterpret_cast<const char *>(m_packet.get() + idx * opt::len), len), opt::copies))
                    {
                        m_fail->fetch_add(1, std::memory_order_relaxed);
                        continue;
                    }
                }
                else
                {
                    if (!m_pfq.send(pfq::const_buffer(reinterpret_cast<const char *>(m_packet.get() + idx * opt::len), len), opt::queue_sync, opt::copies))
                    {
                        m_fail->fetch_add(1, std::memory_order_relaxed);
                        continue;
                    }
                }

                idx++;

                m_sent->fetch_add(1, std::memory_order_relaxed);
                m_band->fetch_add(len, std::memory_order_relaxed);
                m_gros->fetch_add(len+24, std::memory_order_relaxed);

                n++;

                if (opt::stop.load(std::memory_order_relaxed))
                    break;
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

            std::exponential_distribution<double> exp_dist(1.0 / static_cast<double>((delta-pkt_time).count()));

            uint32_t rand_mask = ((1U << opt::rand_depth)-1);

            for(size_t n = 0; n < opt::npackets;)
            {
                if (opt::interactive)
                    wait_keyboard();

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

                if (opt::rand_src_ip)
                {
                    ip->saddr = opt::src_ip | htonl(static_cast<uint32_t>(m_gen()) & rand_mask);
                }
                if (opt::rand_dst_ip)
                {
                    ip->daddr = opt::dst_ip | htonl(static_cast<uint32_t>(m_gen()) & rand_mask);
                }

                n++;

                if (opt::stop.load(std::memory_order_relaxed))
                    break;
            }
        }


#ifdef HAVE_PCAP_H
        void pcap_generator()
        {
            struct pcap_pkthdr *hdr;
            u_char *data;

            auto rc = opt::rate != 0.0;
            uint32_t rand_mask = ((1U << opt::rand_depth)-1);
            auto rand_flow_mask = ((1ULL << opt::rand_flow_depth)-1);

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

                    if (opt::interactive)
                        wait_keyboard();

                    if (auto ip = opt::rand_src_ip ? reinterpret_cast<iphdr *>(data + 14) : nullptr)
                    {
                        ip->saddr = opt::src_ip | htonl(static_cast<uint32_t>(m_gen()) & rand_mask);
                    }
                    if (auto ip = opt::rand_dst_ip ? reinterpret_cast<iphdr *>(data + 14) : nullptr)
                    {
                        ip->daddr = opt::dst_ip | htonl(static_cast<uint32_t>(m_gen()) & rand_mask);
                    }

                    if (auto ip = opt::rand_flow ? reinterpret_cast<iphdr *>(data + 14) : nullptr)
                    {
                        auto hash = ip->saddr ^ ip->daddr;
                        auto seed = opt::rand_seed[(hash+l) & rand_flow_mask];
                        ip->saddr ^= seed;
                        ip->daddr ^= seed;
                    }

                    if (m_async)
                    {
                        if (!m_pfq.send_async(pfq::const_buffer(reinterpret_cast<const char *>(data), plen), opt::copies))
                        {
                            m_fail->fetch_add(1, std::memory_order_relaxed);
                            continue;
                        }
                    }
                    else
                    {
                        if (!m_pfq.send(pfq::const_buffer(reinterpret_cast<const char *>(data), plen), opt::queue_sync, opt::copies))
                        {
                            m_fail->fetch_add(1, std::memory_order_relaxed);
                            continue;
                        }
                    }

                    m_sent->fetch_add(1, std::memory_order_relaxed);
                    m_band->fetch_add(plen, std::memory_order_relaxed);
                    m_gros->fetch_add(plen+24, std::memory_order_relaxed);

                    n = pcap_next_ex(p, &hdr, (u_char const **)&data);
                    if (n == -2)
                        break;

                    i++;

                    if (opt::stop.load(std::memory_order_relaxed))
                        break;
                }

                pcap_close(p);
            }
        }
#endif

        template <typename Tp, typename Dur>
        void rate_control(Tp &now, Dur const &delta, size_t n)
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

        bool m_async;
    };

}


void usage(std::string name)
{
    throw std::runtime_error
    (
        "usage: " + std::move(name) + " [OPTIONS]\n\n"
        " -h --help                     Display this help\n"
        " -l --len INT                  Set packet length\n"
        " -n --packets INT              Number of packets to transmit\n"
        "    --seconds INT              Number of seconds to transmit\n"
        " -c --copies INT               Number of per-packet copies\n"
        " -s --queue-slots INT          Set Tx queue length\n"
        " -k --kthread IDX,IDX...       Async with kernel threads\n"
#ifdef HAVE_PCAP_H
        " -r --read FILE                Read pcap trace file to send\n"
        "    --loop                     Loop through the trace file N times\n"
#endif

        "    --src-ip IP                Source IP address\n"
        "    --dst-ip IP                Dest IP address\n"
        "    --src-port PORT            Source UDP port\n"
        "    --dst-port PORT            Dest UDP address\n"
        " -R --rand-ip                  Randomize IP addresses\n"
        "    --rand-src-ip              Randomize IP source addresses\n"
        "    --rand-dst-ip              Randomize IP dest addresses\n"
        "    --rand-depth               Depth of randomization (0-32)\n"
#ifdef HAVE_PCAP_H
        " -F --rand-flow                Randomize IP addresses per-flow\n"
        "    --rand-flow-depth          Depth of flow-randomization (def. 8)\n"
#endif
        "    --dst-mac MAC              Specify dest MAC address\n"
        "    --src-mac MAC              Specify source MAC address\n"
        " -P --preload INT              Preload INT packets (must be a power of 2)\n"
        "    --rate DOUBLE              Packet rate in Mpps\n"
        "    --interactive              Transmit a packet at time\n"
        " -a --active-tstamp            Use active timestamp as rate control\n"
        " -p --poisson                  Use a Poisson process for inter-packet gaps, implies -a\n"
        " -S --queue-sync INT           Set queue sync value, used to Tx sync\n"
        " -t --thread BINDING\n\n"
        "      " + more::netdev_format + "\n" +
        "      " + more::thread_binding_format
    );
}


std::vector<thread::context *> thread_ctx;

void sighandler(int)
{
    opt::stop.store(true, std::memory_order_relaxed);
}


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

        if ( any_strcmp(argv[i], "--rand-flow-depth") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("number missing");
            }

            opt::rand_depth = static_cast<uint32_t>(std::atoi(argv[i]));

            if (opt::rand_flow_depth > 31)
                throw std::runtime_error("rand-flow-depth: too large value [0-32]!");

            continue;
        }

#endif

        if ( any_strcmp(argv[i], "-S", "--sync") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("hint missing");
            }

            opt::queue_sync = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if ( any_strcmp(argv[i], "-c", "--copies") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("hint missing");
            }

            opt::copies = static_cast<unsigned int>(std::atoi(argv[i]));
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

        if ( any_strcmp(argv[i], "--seconds") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("number missing");
            }

            opt::seconds = static_cast<size_t>(std::atoi(argv[i]));
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

        if ( any_strcmp(argv[i], "--rand-depth") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("number missing");
            }

            opt::rand_depth = static_cast<uint32_t>(std::atoi(argv[i]));

            if (opt::rand_depth > 31)
                throw std::runtime_error("rand-depth: too large value [0-32]!");

            continue;
        }

        if ( any_strcmp(argv[i], "--src-ip") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("IP missing");
            }

            if (inet_pton(AF_INET, argv[i], &opt::src_ip) != 1) {
                throw std::runtime_error("src-ip: bad address!");
            }

            continue;
        }


        if ( any_strcmp(argv[i], "--dst-ip") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("IP missing");
            }

            if (inet_pton(AF_INET, argv[i], &opt::dst_ip) != 1) {
                throw std::runtime_error("dst-ip: bad address!");
            }

            continue;
        }

        if ( any_strcmp(argv[i], "--src-port") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("port missing");
            }

            opt::src_port = static_cast<uint16_t>(atoi(argv[i]));
            continue;
        }


        if ( any_strcmp(argv[i], "--dst-port") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("port missing");
            }

            opt::dst_port = static_cast<uint16_t>(atoi(argv[i]));
            continue;
        }

        if ( any_strcmp(argv[i], "-R", "--rand-ip") )
        {
            opt::rand_src_ip = true;
            opt::rand_dst_ip = true;
            continue;
        }

        if ( any_strcmp(argv[i], "--rand-src-ip") )
        {
            opt::rand_src_ip = true;
            continue;
        }

        if ( any_strcmp(argv[i], "--rand-dst-ip") )
        {
            opt::rand_dst_ip = true;
            continue;
        }

        if ( any_strcmp(argv[i], "--interactive") )
        {
            opt::interactive = true;
            continue;
        }

        if ( any_strcmp(argv[i], "--src-mac") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("MAC missing");
            }

            opt::src_mac = argv[i];
            continue;
        }

        if ( any_strcmp(argv[i], "--dst-mac") )
        {
            if (++i == argc)
            {
                throw std::runtime_error("MAC missing");
            }

            opt::dst_mac = argv[i];
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

    // loading rand seeds...
    //

    std::cout << "rand_ip_src: "  << std::boolalpha << (opt::rand_src_ip) << std::endl;
    std::cout << "rand_ip_dst: "  << std::boolalpha << (opt::rand_dst_ip) << std::endl;

    if (opt::rand_flow)
    {
        auto max_seed = (1ULL <<opt::rand_flow_depth);
        std::mt19937 gen;

        std::cout << "rand_flow  : loading " << max_seed << " seeds..." << std::endl;

        opt::rand_seed.reserve(max_seed);

        for(uint64_t i = 0; i < max_seed; i++)
            opt::rand_seed.push_back(static_cast<uint32_t>(gen()));
    }

    while (opt::kthread.size() < binding.size())
        opt::kthread.push_back(std::vector<int>{});

    //
    // process binding:
    //

    int queue_num = 0;

    for(size_t i = 0; i < binding.size(); ++i)
    {
        if (binding[i].dev.empty())
            throw std::runtime_error("binding error: device unspecified");

        std::cout << "device     : " << show(binding[i].dev.front()) << std::endl;

        while (binding.at(i).dev.front().queue.size() < opt::kthread.at(i).size())
            binding.at(i).dev.front().queue.push_back(queue_num++);

        if (opt::kthread.at(i).empty() && binding.at(i).dev.front().queue.size() > 1)
            throw std::runtime_error("multiple hw queues require async transmission (-k)");

        while (opt::kthread.at(i).size() < binding.at(i).dev.front().queue.size()) {

            if (opt::kthread.at(i).empty() && opt::active_ts)
                throw std::runtime_error("active_ts requires async transmission (-k)");

            opt::kthread.at(i).push_back(opt::kthread.at(i).empty() ? no_kthread : opt::kthread.at(i).back());
        }

        if (binding.at(i).dev.front().queue.empty())
        {
            binding.at(i).dev.front().queue.push_back(-1);
            opt::kthread.at(i).push_back(-1);
        }
    }

    if (opt::slots == 0)
        throw std::runtime_error("tx_slots set to 0!");

    if (opt::rand_flow && opt::file.empty())
        throw std::runtime_error("random flow requires reading packets from file (r)");

    // HugePages warning...
    //

    if (pfq::hugepages_mountpoint().empty())
        std::cout << "*** Warning: HugePages not mounted ***" << std::endl;

    // preloaded packets must be a power of 2
    //

    if (opt::preload & (opt::preload-1))
        throw std::runtime_error("preloaded packets must be a power of 2");

    ////////////////////////////////////////////////////////////////////////////////////


    std::cout << "len        : "  << opt::len << std::endl;
    std::cout << "queue sync : "  << opt::queue_sync << std::endl;

    if (opt::npackets != std::numeric_limits<size_t>::max())
        std::cout << "npackets   : " << opt::npackets << std::endl;
    if (opt::seconds != std::numeric_limits<size_t>::max())
        std::cout << "seconds    : " << opt::seconds << std::endl;

    std::cout << "copies     : "  << opt::copies << std::endl;

    if (opt::rate != 0.0)
        std::cout << "rate       : "  << opt::rate << " Mpps" << std::endl;

    if (opt::active_ts && !opt::poisson)
        std::cout << "timestamp  : active!" << std::endl;

    if (opt::poisson)
        std::cout << "timestamp  : active with poisson Process traffic!" << std::endl;

    auto mq = std::any_of(std::begin(binding), std::end(binding), [](more::thread_binding const &b) { return b.dev.front().queue.size() > 1; });

    if (!opt::rand_src_ip &&
        !opt::rand_dst_ip &&
        mq)
    {
        std::cout << vt100::BOLD << "*** Multiple queue detected! Consider to randomize IP addresses with -R option! ***" << vt100::RESET << std::endl;
    }

    //
    // create thread context:
    //

    for(unsigned int i = 0; i < binding.size(); ++i)
    {
        thread_ctx.push_back(new thread::context(static_cast<int>(i), binding[i], opt::kthread.at(i)));
    }

    opt::nthreads.store(static_cast<int>(binding.size()), std::memory_order_relaxed);

    //
    // create threads:
    //

    size_t i = 0;
    std::for_each(binding.begin(), binding.end(), [&](more::thread_binding const &b) {

        auto t = new std::thread(std::ref(*thread_ctx[i++]));

        more::set_affinity(*t, static_cast<size_t>(b.cpu));

        t->detach();
    });

    pfq_stats cur, prec = {0,0,0,0,0,0,0,0};

    uint64_t sent, sent_ = 0;
    uint64_t band, band_ = 0;
    uint64_t gros, gros_ = 0;
    uint64_t fail, fail_ = 0;

    signal(SIGINT, sighandler);

    std::cout << "------------ gen started ------------\n";

    auto begin = std::chrono::system_clock::now();

    for(size_t y=0; y < opt::seconds; y++)
    {
        std::this_thread::sleep_for(std::chrono::seconds(1));

        cur = {0,0,0,0,0,0,0,0};
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

        if (!opt::interactive)
        {
            std::cout << "stats    : { " << cur << " }" << std::endl;

            std::cout << "   app   : { "
                      << vt100::BOLD
                      << "sent: " << persecond<int64_t>(sent - sent_, delta)            << ' '
                      << "fail: " << persecond<int64_t>(fail-fail_, delta)              << " ";
            if (opt::copies > 1)
               std::cout << " (x " << opt::copies << ") => ";

            std::cout << "band: " << pretty_number(persecond<double>((band-band_)*8 * opt::copies, delta))  << "bit/sec "
                      << "gros: " << pretty_number(persecond<double>((gros-gros_)*8 * opt::copies, delta))  << "bit/sec "
                      << vt100::RESET << " }" << std::endl;

            std::cout << "   socket: { "
                      << "sent: " << vt100::BOLD << persecond<int64_t>(cur.sent - prec.sent, delta) << vt100::RESET << " pkt/sec "
                      << "disc: " << vt100::BOLD << persecond<int64_t>(cur.disc - prec.disc, delta) << vt100::RESET << " pkt/sec "
                      << "fail: " << vt100::BOLD << persecond<int64_t>(cur.fail - prec.fail, delta) << vt100::RESET << " pkt/sec "
                      << " }" << std::endl;
        }

        prec = cur, begin = end;
        sent_ = sent;
        band_ = band;
        gros_ = gros;
        fail_ = fail;

        if (opt::nthreads.load(std::memory_order_relaxed) == 0 ||
            opt::stop.load(std::memory_order_relaxed))
            break;
    }

    std::cout << "Shutting down sockets in 1 sec..." << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(1));

    cur  = {0,0,0,0,0,0,0,0};
    sent = 0;

    std::for_each(thread_ctx.begin(), thread_ctx.end(), [&](const thread::context *c)
    {
        auto p = c->stats();

        cur  += std::get<0>(p);
        sent += std::get<1>(p);
    });

    std::cout << "Summary:" << std::endl;

    std::cout << "    PFQ packets sent:" << cur.sent << " discarded:" << cur.disc << " attempt:" << cur.fail << std::endl;
    std::cout << "    App packets sent:" << sent << std::endl;

}
catch(std::exception &e)
{
    std::cerr << "pfq-gen: " << e.what() << std::endl;
}
