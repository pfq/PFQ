/***************************************************************
 *
 * (C) 2011 - Nicola Bonelli <nicola@pfq.io>
 *            Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 *
 ****************************************************************/

#include <iostream>
#include <fstream>
#include <sstream>

#include <iomanip>
#include <cstdio>
#include <thread>
#include <chrono>
#include <string>
#include <cstring>
#include <vector>
#include <algorithm>
#include <iterator>
#include <atomic>
#include <set>
#include <cmath>
#include <csignal>
#include <tuple>
#include <limits>
#include <unordered_map>

#include <pfq/pfq.hpp>
#include <pfq/lang/lang.hpp>
#include <pfq/lang/default.hpp>
#include <pfq/lang/experimental.hpp>

#include <more/binding.hpp>
#include <more/affinity.hpp>
#include <more/vt100.hpp>
#include <more/pretty.hpp>

#include <linux/ip.h>
#include <linux/udp.h>

using namespace more;

using namespace pfq;
using namespace pfq::lang;
using namespace pfq::lang::experimental;


namespace opt
{
    ///////////////////////////////////////////

    auto comp = unit;

    ///////////////////////////////////////////

    long timeout_ms;
    std::string function;

    size_t seconds = std::numeric_limits<size_t>::max();
    size_t caplen  = 64;
    size_t slots   = 131072;

    bool flow      = false;
    bool use_comp  = false;
    bool promisc   = true;
    bool dump      = false;

    std::string dumpfile;

    std::atomic_bool stop;
}


typedef std::tuple<uint32_t, uint32_t, uint16_t, uint16_t, uint8_t> Tuple;


namespace flow
{
    std::atomic_int  rop;

    struct HashTuple
    {
        uint32_t operator()(Tuple const &t) const
        {

            return std::get<0>(t) ^ std::get<1>(t) ^ std::get<2>(t) ^ (static_cast<uint32_t>(std::get<3>(t)) << 16);
        }
    };

    struct state
    {
        size_t  count;
        size_t  bytes;
        size_t  count_dscp[64];
        size_t  bytes_dscp[64];
    };
}


namespace thread
{
    namespace pcap_emu
    {
        uint32_t constexpr magic_number = 0xA1B2C3D4;
        int constexpr zone_gmt = 0;

        struct file_header
        {
            uint32_t magic;
            unsigned short version_major;
            unsigned short version_minor;
            int32_t thiszone;	/* gmt to local correction */
            uint32_t sigfigs;	/* accuracy of timestamps */
            uint32_t snaplen;	/* max length saved portion of each pkt */
            uint32_t linktype;	/* data link type (LINKTYPE_*) */
        };

        struct pkthdr
        {
            uint32_t sec;
            uint32_t usec;
            uint32_t caplen;
            uint32_t len;
        };
    }

    struct context
    {
        using FlowMap = std::unordered_map<std::tuple<uint32_t, uint32_t, uint16_t, uint16_t, uint8_t>, flow::state, flow::HashTuple>;

        context(int id, const thread_binding &b)
        : m_id(id)
        , m_bind(b)
        , m_pfq(group_policy::undefined, opt::caplen, opt::slots)
        , m_read()
        , m_batch()
        , m_flow_map()
        , m_filename()
        , m_file(nullptr)
        {
            if (m_bind.gid == -1)
                m_bind.gid = id;

            if (!opt::dumpfile.empty())
            {
                auto fs = pfq::split(opt::dumpfile, ".");
                m_filename = fs.at(0) + ".pfq." + std::to_string(id) + (fs.size() > 1 ? ("." + fs.at(1)) : ".pcap");
            }

            m_pfq.join_group(m_bind.gid, group_policy::shared);

            for(auto &d : m_bind.dev)
            {
                if (d.queue.empty())
                {
                    m_pfq.bind_group(m_bind.gid, d.name.c_str(), -1);
                    std::cout << "+ bind to " << d.name << "@" << -1 << std::endl;
                }
                else
                    for(auto q : d.queue)
                    {
                        std::cout << "+ bind to " << d.name << "@" << q << std::endl;
                        m_pfq.bind_group(m_bind.gid, d.name.c_str(), q);
                    }
            }

            if (opt::use_comp)
            {
                std::cout << "computation: " << pretty (opt::comp) << std::endl;
                m_pfq.set_group_computation(m_bind.gid, opt::comp);
            }

            if (!opt::function.empty() && (m_id == 0))
            {
                std::cout << "function: " << opt::function << std::endl;
                m_pfq.set_group_computation(m_bind.gid, opt::function);
            }

            m_pfq.timestamping_enable(false);
        }

        context(const context &) = delete;
        context& operator=(const context &) = delete;

        context(context &&) = default;
        context& operator=(context &&) = default;

        void operator()()
        {
            m_pfq.enable();

            if (!m_filename.empty() || opt::dump)
            {
                m_pfq.timestamping_enable(true);
            }

            if (!m_filename.empty())
            {
                pcap_open_();
            }

            const bool dump = opt::dump || m_file;

            for(;;)
            {
                auto many = m_pfq.read(opt::timeout_ms);

                auto this_rop = flow::rop.load(std::memory_order_acquire);

                m_read += many.size();
                m_batch = std::max(m_batch, many.size());

                if (dump) {

                    auto it = many.begin();
                    for(; it != many.end(); ++it)
                    {
                        while (!it.ready())
                            std::this_thread::yield();

                        auto & h = *it;
                        const unsigned char *buff = static_cast<unsigned char *>(it.data());

                        if (m_file)
                            pcap_write_(buff, h.len, h.caplen, h.tstamp.tv.sec, h.tstamp.tv.nsec/1000);

                        if (opt::dump) {
                            printf("%d:%d [%d] (%d/%d)",
                                            h.tstamp.tv.sec,
                                            h.tstamp.tv.nsec,
                                            h.info.ifindex,
                                            h.caplen,
                                            h.len);
                            printf("\n    |");
                            for(auto n = 0U; n < std::min<size_t>(14, h.caplen); n++)
                                printf("%02x ", buff[n]);
                            printf("\n    |");
                            for(auto n = 14U; n < std::min<size_t>(34, h.caplen); n++)
                                printf("%02x ", buff[n]);
                            printf("...\n");
                        }
                    }
                }


                if (opt::flow)
                {
                    auto it = many.begin();
                    auto it_e = many.end();
                    for(; it != it_e; ++it)
                    {
                        while(!it.ready())
                            std::this_thread::yield();

                        auto & h = *it;

                        iphdr  * ipv4 = reinterpret_cast<iphdr *> (static_cast<char *>(it.data()) + 14);
                        if (ipv4->protocol == IPPROTO_TCP || ipv4->protocol == IPPROTO_UDP)
                        {
                            udphdr * udp = reinterpret_cast<udphdr *>(static_cast<char *>(it.data()) + (ipv4->ihl<<2));

                            auto key = std::make_tuple(ipv4->saddr, ipv4->daddr, udp->source, udp->dest, ipv4->protocol);
                            auto this_flow = m_flow_map[this_rop & 1].find(key);
                            if (this_flow == m_flow_map[this_rop & 1].end()) {
                                std::tie(this_flow, std::ignore) = m_flow_map[this_rop & 1].insert(std::make_pair(key, flow::state{}));
                            }

                            auto dscp = ntohs(ipv4->tos) >> 2;

                            this_flow->second.count++;
                            this_flow->second.bytes+= h.len;
                            this_flow->second.count_dscp[dscp]++;
                            this_flow->second.bytes_dscp[dscp] += h.len;
                        }
                    }
                }

                if (opt::stop.load(std::memory_order_relaxed))
                    break;
            }

            if (m_file)
                pcap_close_();

            m_pfq.disable();
        }

        pfq_stats
        stats() const
        {
            return m_pfq.stats();
        }

        unsigned long long
        read() const
        {
            return m_read;
        }


        FlowMap &
        flow_map()
        {
            auto rop = flow::rop.load(std::memory_order_relaxed) + 1;
            return m_flow_map[rop & 1];
        }

        FlowMap const &
        flow_map() const
        {
            auto rop = flow::rop.load(std::memory_order_relaxed) + 1;
            return m_flow_map[rop & 1];
        }

        size_t
        batch() const
        {
            return m_batch;
        }

    private:

        void pcap_open_()
        {
            m_file = fopen(m_filename.c_str(),"wb");
            if (m_file == nullptr)
                throw std::runtime_error("pfq: could not open " + m_filename);

            pcap_emu::file_header header;

            header.magic = pcap_emu::magic_number;
            header.version_major = 2;
            header.version_minor = 4;
            header.thiszone = pcap_emu::zone_gmt + 1;
            header.sigfigs = 0;
            header.snaplen = 0xFFFF;
            header.linktype = 1; // ethernet;

            fwrite (&header, sizeof (header), 1, m_file);
            fflush (m_file);
        }

        void pcap_close_()
        {
            ::fclose(m_file);
            m_file = nullptr;
        }

        void pcap_write_(const unsigned char *ptr, size_t len, size_t caplen, uint32_t sec, uint32_t usec)
        {
            pcap_emu::pkthdr header;

            header.sec = sec;
            header.usec = usec;
            header.caplen = (uint32_t)caplen;
            header.len = (uint32_t)len;

            fwrite (&header, sizeof (header), 1, m_file);
            fwrite (ptr, 1, caplen, m_file);
            fflush (m_file);
        }

        int m_id;
        thread_binding m_bind;

        pfq::socket m_pfq;

        unsigned long long m_read;
        size_t m_batch;

        FlowMap m_flow_map[2];

        std::string m_filename;
        FILE * m_file;
    };

}


void usage(std::string name)
{
    throw std::runtime_error
    (
        "usage: " + std::move(name) + " [OPTIONS]\n\n"
        " -h --help                     Display this help\n"
        " -c --caplen INT               Set caplen\n"
        " -w --write FILE               Write packets to file (pcap)\n"
        " -d --dump                     Dump packets to stdout\n"
        "    --flow                     Enable per-flow counters\n"
        " -s --slot INT                 Set slots\n"
        "    --seconds INT              Terminate after INT seconds\n"
        "    --no-promisc               Disable promiscuous mode (enabled by default)\n"
        " -f --function FUNCTION\n"
        " -t --thread BINDING\n\n"
        "      " + more::netdev_format + "\n"
        "      " + more::thread_binding_format + "\n"
        "      FUNCTION = fun[ >-> fun >-> fun]"
    );
}


std::vector<thread::context *> thread_ctx;

void sighandler(int)
{
    opt::stop.store(true, std::memory_order_relaxed);
}


inline std::string show_ip(uint32_t addr)
{
    char ip[16];
    if (inet_ntop(AF_INET, &addr, ip, sizeof(ip)) == nullptr)
        throw std::runtime_error("address::operator<<");
    return ip;
}


int
main(int argc, char *argv[])
try
{
    if (argc < 2)
        usage(argv[0]);

    std::vector<thread_binding> thread_bindings;

    for(int i = 1; i < argc; ++i)
    {
        if (any_strcmp(argv[i], "-f", "--fun"))
        {
            if (++i == argc)
                throw std::runtime_error("group function missing");

            opt::function.assign(argv[i]);
            continue;
        }

        if (any_strcmp(argv[i], "-C", "--comp"))
        {
            opt::use_comp = true;
            continue;
        }

        if (any_strcmp(argv[i], "-c", "--caplen"))
        {
            if (++i == argc)
                throw std::runtime_error("caplen missing");

            opt::caplen = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if (any_strcmp(argv[i], "-s", "--slots"))
        {
            if (++i == argc)
                throw std::runtime_error("slots missing");

            opt::slots = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if (any_strcmp(argv[i], "--seconds"))
        {
            if (++i == argc)
                throw std::runtime_error("seconds missing");

            opt::seconds = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if (any_strcmp(argv[i], "-l", "--flow"))
        {
            opt::flow = true;
            continue;
        }

        if (any_strcmp(argv[i], "-w", "--write"))
        {
            if (++i == argc)
                throw std::runtime_error("filename missing");

            opt::dumpfile = argv[i];
            continue;
        }

        if (any_strcmp(argv[i], "-d", "--dump"))
        {
            opt::dump = true;
            continue;
        }

        if (any_strcmp(argv[i], "-t", "--thread"))
        {
            if (++i == argc)
                throw std::runtime_error("descriptor missing");

            thread_bindings.push_back(read_thread_binding(argv[i]));
            continue;
        }

        if ( any_strcmp(argv[i], "--no-promisc") )
        {
            opt::promisc = false;
            continue;
        }

        if (any_strcmp(argv[i], "-h", "-?", "--help"))
            usage(argv[0]);

        throw std::runtime_error(std::string(argv[i]) + " unknown option!");
    }

    std::cout << "caplen: " << opt::caplen << std::endl;
    std::cout << "slots : " << opt::slots << std::endl;

    if (opt::slots < 64)
    {
        std::cout << "too few slots may reduce performance!" << std::endl;
        _Exit(0);
    }

    // create thread context:
    //
    for(unsigned int i = 0; i < thread_bindings.size(); ++i)
    {
        thread_ctx.push_back(new thread::context(static_cast<int>(i), thread_bindings[i]));
    }

    opt::timeout_ms = 1000000;

    std::cout << "poll timeout " << opt::timeout_ms << " usec" << std::endl;

    // set promiscuous mode...
    //

    {
        auto sd = ::socket(AF_INET, SOCK_DGRAM, 0);
        if (sd == -1)
            throw std::runtime_error("could not open a SOCK_DGRAM");

        std::set<std::string> devs;

        for(auto & tb : thread_bindings)
        {
            for(auto & d : tb.dev)
            {
                devs.insert(d.name);
            }
        }

        std::cout << std::boolalpha;
        for(auto & d : devs)
        {
            set_promisc(sd, d.c_str(), opt::promisc);
            std::cout << "+ promiscuous: " << d <<
                         " (" << ( opt::promisc ? "enabled" : "disabled") << ")" << std::endl;
        }

        ::close(sd);
    }

    // create threads:
    //

    int i = 0;
    std::for_each(thread_bindings.begin(), thread_bindings.end(), [&](thread_binding &b) {

        std::cout << "thread: " << show(b) << std::endl;

        auto t = new std::thread(std::ref(*thread_ctx[static_cast<size_t>(i++)]));

        if (b.cpu != -1)
            more::set_affinity(*t, static_cast<size_t>(b.cpu));

        t->detach();
    });

    // HugePages warning...
    //

    if (pfq::hugepages_mountpoint().empty())
        std::cout << "*** Warning: HugePages not mounted ***" << std::endl;

    unsigned long long sum, flow, old = 0;
    pfq_stats sum_stats, old_stats = {0,0,0,0,0,0,0,0};

    signal(SIGINT, sighandler);

    std::cout << "----------- capture started ------------\n";

    auto begin = std::chrono::system_clock::now();

    for(size_t y=0; y < opt::seconds; y++)
    {
        flow::rop.fetch_add(1, std::memory_order_release);

        std::this_thread::sleep_for(std::chrono::seconds(1));

        if (opt::stop.load(std::memory_order_relaxed))
            break;

        if (opt::dump)
            continue;

        sum = 0;
        flow = 0;
        sum_stats = {0,0,0,0,0,0,0,0};

        std::for_each(thread_ctx.begin(), thread_ctx.end(), [&](const thread::context *c) {
            sum += c->read();
            flow += c->flow_map().size();
            sum_stats += c->stats();
        });

        std::cout << "recv      : ";
        std::for_each(thread_ctx.begin(), thread_ctx.end(), [&](const thread::context *c) {
                      std::cout << c->stats().recv << ' ';
                      });
        std::cout << " -> " << sum_stats.recv << std::endl;

        std::cout << "lost      : ";
        std::for_each(thread_ctx.begin(), thread_ctx.end(), [&](const thread::context *c) {
                      std::cout << c->stats().lost << ' ';
                      });
        std::cout << " -> " << sum_stats.lost << std::endl;

        std::cout << "drop      : ";
        std::for_each(thread_ctx.begin(), thread_ctx.end(), [&](const thread::context *c) {
                      std::cout << c->stats().drop << ' ';
                      });
        std::cout << " -> " << sum_stats.drop << std::endl;

        std::cout << "max_batch : ";
        std::for_each(thread_ctx.begin(), thread_ctx.end(), [&](const thread::context *c) {
                      std::cout << c->batch() << ' ';
                      });
        std::cout << std::endl;

        auto end = std::chrono::system_clock::now();

        auto rate = (static_cast<int64_t>(sum-old)*1000000)/
                        std::chrono::duration_cast<std::chrono::microseconds>(end-begin).count();

        std::cout << "capture   : " << vt100::BOLD << pretty_number<double>(rate) << " pkt/sec" << vt100::RESET << std::endl;

        if (opt::flow) {

            auto dur_now = end.time_since_epoch();

            std::cout << "timestamp : " << std::chrono::duration_cast<std::chrono::seconds>(dur_now).count() << ':' 
                                        << '.' << std::setfill('0') << std::setw(9) << (std::chrono::duration_cast<std::chrono::nanoseconds>(dur_now).count() % 1000000)
                                        << std::endl;

            std::cout << "flows     : " << flow << std::endl;
            for(auto &ctx : thread_ctx) {

                auto & fmap = ctx->flow_map();
                for(auto & f : fmap) {

                    std::cout << "    "
                              << static_cast<int>(std::get<4>(f.first)) << "|"
                              << show_ip(std::get<0>(f.first)) << ":"
                              << ntohs(std::get<2>(f.first))   << " -> "
                              << show_ip(std::get<1>(f.first)) << ":"
                              << ntohs(std::get<3>(f.first))   << "\t"
                              << pretty_number<double>(f.second.count) << "pkt/sec - "
                              << pretty_number<double>(f.second.bytes * 8) << "bit/sec - ";

                    for(int i = 0; i < 64; i++) {
                        auto & cnt = f.second.count_dscp[i];
                        auto & byt = f.second.bytes_dscp[i];
                        if (cnt) {
                            std::cout << std::hex << i << std::dec << ":{ pkt " << cnt << ", " << pretty_number<double>(byt*8) << "bit/sec } ";
                            cnt = 0;
                            byt = 0;
                        }
                    }

                    std::cout << std::endl;

                    f.second.count = 0;
                    f.second.bytes = 0;
                }
            }
        }

        old = sum, begin = end;
        old_stats = sum_stats;
    }

    std::cout << "closing...";
    std::this_thread::sleep_for(std::chrono::seconds(1));
}
catch(std::exception &e)
{
    std::cerr << e.what() << std::endl;
}
