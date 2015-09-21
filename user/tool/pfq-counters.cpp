/***************************************************************
 *
 * (C) 2011 - Nicola Bonelli <nicola@pfq.io>
 *            Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 *
 ****************************************************************/

#include <iostream>
#include <fstream>
#include <sstream>

#include <cstdio>
#include <thread>
#include <string>
#include <cstring>
#include <vector>
#include <algorithm>
#include <iterator>
#include <atomic>
#include <cmath>
#include <tuple>
#include <limits>
#include <unordered_set>

#include <pfq/pfq.hpp>
#include <pfq/lang/lang.hpp>
#include <pfq/lang/default.hpp>
#include <pfq/lang/experimental.hpp>

#include <more/binding.hpp>
#include <more/affinity.hpp>
#include <more/vt100.hpp>

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

    std::string dumpfile;
}


typedef std::tuple<uint32_t, uint32_t, uint16_t, uint16_t> Tuple;

struct HashTuple
{
    uint32_t operator()(Tuple const &t) const
    {

        return std::get<0>(t) ^ std::get<1>(t) ^ std::get<2>(t) ^ (static_cast<uint32_t>(std::get<3>(t)) << 16);
    }
};


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
        context(int id, const thread_binding &b)
        : m_id(id)
        , m_bind(b)
        , m_pfq(group_policy::undefined, opt::caplen, opt::slots)
        , m_read()
        , m_batch()
        , m_set()
        , m_flow()
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
            m_pfq.enable();
        }

        context(const context &) = delete;
        context& operator=(const context &) = delete;

        context(context &&) = default;
        context& operator=(context &&) = default;

        void operator()()
        {
            if (!m_filename.empty())
            {
                m_pfq.timestamping_enable(true);
                pcap_open_();
            }

            for(;;)
            {
                auto many = m_pfq.read(opt::timeout_ms);

                m_read += many.size();
                m_batch = std::max(m_batch, many.size());

                if (m_file) {

                    auto it = many.begin();
                    for(; it != many.end(); ++it)
                    {
                        while (!it.ready())
                            std::this_thread::yield();

                        auto h = *it;
                        const char *buff = static_cast<char *>(it.data());

                        pcap_write_(buff, h.len, h.caplen, h.tstamp.tv.sec, h.tstamp.tv.nsec/1000);
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

                        iphdr  * ipv4 = reinterpret_cast<iphdr *> (static_cast<char *>(it.data()) + 14);
                        if (ipv4->protocol == IPPROTO_TCP || ipv4->protocol == IPPROTO_UDP)
                        {
                            udphdr * udp = reinterpret_cast<udphdr *>(static_cast<char *>(it.data()) + (ipv4->ihl<<2));

                            if (m_set.insert(std::make_tuple(ipv4->saddr, ipv4->daddr, udp->source, udp->dest)).second)
                                m_flow++;
                        }
                    }
                }
            }

            if (m_file)
                pcap_close_();
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

        unsigned long
        flow() const
        {
            return m_flow;
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

        void pcap_write_(const char *ptr, size_t len, size_t caplen, uint32_t sec, uint32_t usec)
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

        std::unordered_set<std::tuple<uint32_t, uint32_t, uint16_t, uint16_t>, HashTuple> m_set;

        unsigned long m_flow;

        std::string m_filename;
        FILE * m_file;
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


void usage(std::string name)
{
    throw std::runtime_error
    (
        "usage: " + std::move(name) + " [OPTIONS]\n\n"
        " -h --help                     Display this help\n"
        " -c --caplen INT               Set caplen\n"
        " -w --write FILE               Write packets to file (pcap)\n"
        " -l --flow                     Enable flow counter\n"
        " -s --slot INT                 Set slots\n"
        "    --seconds INT              Terminate after INT seconds\n"
        " -f --function FUNCTION\n"
        " -t --thread BINDING\n\n"
        "      " + more::netdev_format + "\n"
        "      " + more::thread_binding_format + "\n"
        "      FUNCTION = fun[ >-> fun >-> fun]"
    );
}


std::vector<thread::context *> thread_ctx;


template <typename T>
std::string
pretty_value(T value)
{
    if (value > 1000000)
        return std::to_string(static_cast<double>(value)/1000000) + "M";
    return std::to_string(value);
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

        if (any_strcmp(argv[i], "-t", "--thread"))
        {
            if (++i == argc)
                throw std::runtime_error("descriptor missing");

            thread_bindings.push_back(read_thread_binding(argv[i]));
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

    // create threads:

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
    pfq_stats sum_stats, old_stats = {0,0,0,0,0,0,0};

    std::cout << "----------- capture started ------------\n";

    auto begin = std::chrono::system_clock::now();

    for(size_t y=0; y < opt::seconds; y++)
    {
        std::this_thread::sleep_for(std::chrono::seconds(1));

        sum = 0;
        flow = 0;
        sum_stats = {0,0,0,0,0,0,0};

        std::for_each(thread_ctx.begin(), thread_ctx.end(), [&](const thread::context *c) {
            sum += c->read();
            flow += c->flow();
            sum_stats += c->stats();
        });

        std::cout << "recv: ";
        std::for_each(thread_ctx.begin(), thread_ctx.end(), [&](const thread::context *c) {
                      std::cout << c->stats().recv << ' ';
                      });
        std::cout << " -> " << sum_stats.recv << std::endl;

        std::cout << "lost: ";
        std::for_each(thread_ctx.begin(), thread_ctx.end(), [&](const thread::context *c) {
                      std::cout << c->stats().lost << ' ';
                      });
        std::cout << " -> " << sum_stats.lost << std::endl;

        std::cout << "drop: ";
        std::for_each(thread_ctx.begin(), thread_ctx.end(), [&](const thread::context *c) {
                      std::cout << c->stats().drop << ' ';
                      });
        std::cout << " -> " << sum_stats.drop << std::endl;

        std::cout << "max_batch: ";
        std::for_each(thread_ctx.begin(), thread_ctx.end(), [&](const thread::context *c) {
                      std::cout << c->batch() << ' ';
                      });
        std::cout << std::endl;

        auto end = std::chrono::system_clock::now();

        auto rate = (static_cast<int64_t>(sum-old)*1000000)/
                        std::chrono::duration_cast<std::chrono::microseconds>(end-begin).count();

        std::cout << "capture: " << vt100::BOLD << pretty_value(rate) << " pkt/sec" << vt100::RESET;

        if (flow) {
            std::cout << " flow: " << flow;
        }

        std::cout << std::endl;

        old = sum, begin = end;
        old_stats = sum_stats;
    }
}
catch(std::exception &e)
{
    std::cerr << e.what() << std::endl;
}
