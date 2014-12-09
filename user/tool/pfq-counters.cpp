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

#include <pfq/pfq.hpp>
#include <pfq/lang/lang.hpp>

#include <binding.hpp>
#include <affinity.hpp>
#include <vt100.hpp>

#include <linux/ip.h>
#include <linux/udp.h>

using namespace pfq;
using namespace pfq::lang;


namespace opt
{
    long timeout_ms;
    std::string function;

    size_t caplen = 64;
    size_t slots  = 131072;
    bool flow     = false;

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
    struct context
    {
        context(int id, const binding &b)
        : m_id(id)
        , m_bind(b)
        , m_stop(std::unique_ptr<std::atomic_bool>(new std::atomic_bool(false)))
        , m_pfq(group_policy::undefined, opt::caplen, opt::slots)
        , m_read()
        , m_batch()
        , m_set()
        , m_flow()
        {
            if (m_bind.gid == -1)
                m_bind.gid = id;

            m_pfq.join_group(m_bind.gid, group_policy::shared);

            for(auto &d : m_bind.dev)
            {
                if (m_bind.queue.empty())
                {
                    m_pfq.bind_group(m_bind.gid, d.c_str(), -1);
                    std::cout << "+ bind to " << d << "@" << -1 << std::endl;
                }
                else
                    for(auto q : m_bind.queue)
                    {
                        std::cout << "+ bind to " << d << "@" << q << std::endl;
                        m_pfq.bind_group(m_bind.gid, d.c_str(), q);
                    }
            }

            if (!opt::function.empty() && (m_id == 0))
            {
                std::cout << "fun: " << opt::function << std::endl;

                m_pfq.set_group_computation(m_bind.gid, opt::function);
            }

            m_pfq.timestamp_enable(false);

            m_pfq.enable();
        }

        context(const context &) = delete;
        context& operator=(const context &) = delete;

        context(context &&) = default;
        context& operator=(context &&) = default;

        void operator()()
        {
            for(;;)
            {
                auto many = m_pfq.read(opt::timeout_ms);

                m_read += many.size();

                m_batch = std::max(m_batch, many.size());

                if (m_stop->load(std::memory_order_relaxed))
                    return;

                if (opt::flow)
                {
                    auto it = many.begin();
                    auto it_e = many.end();
                    for(; it != it_e; ++it)
                    {
                        while(!it.ready())
                            std::this_thread::yield();

                        iphdr  *  ipv4 = static_cast<iphdr *> (it.data());
                        if (ipv4->protocol == IPPROTO_TCP ||
                            ipv4->protocol == IPPROTO_UDP)
                        {
                            udphdr * udp = reinterpret_cast<udphdr *>(static_cast<char *>(it.data()) + (ipv4->ihl<<2));

                            if (m_set.insert(std::make_tuple(ipv4->saddr, ipv4->daddr, udp->source, udp->dest)).second)
                                m_flow++;
                        }
                    }
                }
            }
        }

        void stop()
        {
            m_stop->store(true, std::memory_order_release);
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
        int m_id;
        binding m_bind;

        std::unique_ptr<std::atomic_bool> m_stop;

        pfq::socket m_pfq;

        unsigned long long m_read;
        size_t m_batch;

        std::unordered_set<std::tuple<uint32_t, uint32_t, uint16_t, uint16_t>, HashTuple> m_set;

        unsigned long m_flow;
    };

}


void usage(std::string name)
{
    throw std::runtime_error
    (
        "usage: " + std::move(name) + " [OPTIONS]\n\n"
        " -h --help                     Display this help\n"
        " -c --caplen INT               Set caplen\n"
        " -w --flow                     Enable flow counter\n"
        " -s --slot INT                 Set slots\n"
        " -f --function FUNCTION\n"
        " -t --thread BINDING\n\n"
        "      BINDING = " + pfq::binding_format + "\n"
        "      FUNCTION = fun[ >-> fun >-> fun]"
    );
}


int
main(int argc, char *argv[])
try
{
    if (argc < 2)
        usage(argv[0]);

    std::vector<std::thread> vt;
    std::vector<thread::context> ctx;

    std::vector<binding> thread_binding;

    for(int i = 1; i < argc; ++i)
    {
        if ( strcmp(argv[i], "-f") == 0 ||
             strcmp(argv[i], "--fun") == 0) {
            i++;
            if (i == argc)
            {
                throw std::runtime_error("group function missing");
            }
            opt::function.assign(argv[i]);
            continue;
        }

        if ( strcmp(argv[i], "-c") == 0 ||
             strcmp(argv[i], "--caplen") == 0) {
            i++;
            if (i == argc)
            {
                throw std::runtime_error("caplen missing");
            }

            opt::caplen = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if ( strcmp(argv[i], "-s") == 0 ||
             strcmp(argv[i], "--slots") == 0) {
            i++;
            if (i == argc)
            {
                throw std::runtime_error("slots missing");
            }

            opt::slots = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if ( strcmp(argv[i], "-w") == 0 ||
             strcmp(argv[i], "--flow") == 0)
        {
            opt::flow = true;
            continue;
        }

        if ( strcmp(argv[i], "-t") == 0 ||
             strcmp(argv[i], "--thread") == 0) {
            i++;
            if (i == argc)
            {
                throw std::runtime_error("descriptor missing");
            }

            thread_binding.push_back(make_binding(argv[i]));
            continue;
        }

        if ( strcmp(argv[i], "-h") == 0 ||
             strcmp(argv[i], "-?") == 0 ||
             strcmp(argv[i], "--help") == 0
             )
            usage(argv[0]);

        throw std::runtime_error(std::string(argv[i]) + " unknown option!");
    }

    std::cout << "caplen: " << opt::caplen << std::endl;
    std::cout << "slots : " << opt::slots << std::endl;

    if (opt::slots < 1024)
    {
        std::cout << "too few slots may reduce performance!" << std::endl;
        _Exit(0);
    }

    // create thread context:
    //
    for(unsigned int i = 0; i < thread_binding.size(); ++i)
    {
        ctx.push_back(thread::context(static_cast<int>(i), thread_binding[i]));
    }

    opt::timeout_ms = 1000000;

    std::cout << "poll timeout " << opt::timeout_ms << " usec" << std::endl;

    // create threads:

    int i = 0;
    std::for_each(thread_binding.begin(), thread_binding.end(), [&](binding &b) {

                  std::thread t(std::ref(ctx[static_cast<size_t>(i++)]));

                  std::cout << "thread: " << show_binding(b) << std::endl;

                  if (b.core != -1)
                  extra::set_affinity(t, b.core);

                  vt.push_back(std::move(t));
                  });

    unsigned long long sum, flow, old = 0;
    pfq_stats sum_stats, old_stats = {0,0,0,0,0,0,0};

    std::cout << "----------- capture started ------------\n";

    auto begin = std::chrono::system_clock::now();

    for(int y=0;; y++)
    {
        std::this_thread::sleep_for(std::chrono::seconds(1));

        sum = 0;
        flow = 0;
        sum_stats = {0,0,0,0,0,0,0};

        std::for_each(ctx.begin(), ctx.end(), [&](const thread::context &c) {
                      sum += c.read();
                      flow += c.flow();
                      sum_stats += c.stats();
                      });

        std::cout << "recv: ";
        std::for_each(ctx.begin(), ctx.end(), [&](const thread::context &c) {
                      std::cout << c.stats().recv << ' ';
                      });
        std::cout << " -> " << sum_stats.recv << std::endl;

        std::cout << "lost: ";
        std::for_each(ctx.begin(), ctx.end(), [&](const thread::context &c) {
                      std::cout << c.stats().lost << ' ';
                      });
        std::cout << " -> " << sum_stats.lost << std::endl;

        std::cout << "drop: ";
        std::for_each(ctx.begin(), ctx.end(), [&](const thread::context &c) {
                      std::cout << c.stats().drop << ' ';
                      });
        std::cout << " -> " << sum_stats.drop << std::endl;

        std::cout << "max_batch: ";
        std::for_each(ctx.begin(), ctx.end(), [&](const thread::context &c) {
                      std::cout << c.batch() << ' ';
                      });
        std::cout << std::endl;

        auto end = std::chrono::system_clock::now();

        std::cout << "capture: " << vt100::BOLD <<
        (static_cast<int64_t>(sum-old)*1000000)/std::chrono::duration_cast<std::chrono::microseconds>(end-begin).count()
        << vt100::RESET << " pkt/sec";

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
