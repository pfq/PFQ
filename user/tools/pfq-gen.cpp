/***************************************************************
 *
 * (C) 2011 - Nicola Bonelli <nicola.bonelli@cnit.it>
 *            Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 *
 ****************************************************************/

#include <affinity.hpp>

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
#include <random>

#include <pfq.hpp>
#include <pfq-lang.hpp>

#include <netinet/ip.h>
#include <netinet/udp.h>


char *packet = nullptr;

char *make_packet(size_t n)
{
    auto p = new char[n];

    static const unsigned char ping[98] =
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

    memcpy(p, ping, std::min(n, sizeof(ping)));

    for(auto i = sizeof(ping); i < n; i++)
    {
        p[i] = 0x38 + i - sizeof(ping);
    }

    return p;
}


namespace opt
{
    size_t len    = 64;
    bool  async   = false;
    bool  rand_ip = false;
}


std::vector<std::string>
split(const char *value, char c)
{
    const char * p   = value;
    const char * end = value + strlen(value);

    const char * q;

    std::vector<std::string> ret;

    for(; (q = std::find(p, end, c)) != end; )
    {
        ret.emplace_back(std::string(p, q));
        p = q + 1;
    }

    if (p != end)
        ret.emplace_back(p);

    return ret;
}

// eth0:...:ethx[.core.gid.queue]]

struct binding
{
    std::vector<std::string>    dev;
    std::vector<int>            queue;
};


std::string
show_binding(const binding &b)
{
    std::string ret = "binding:{ ";
    int n = 0;

    ret += "dev:[";

    for(auto &d : b.dev)
    {
        if (n++)
            ret += ", ";
        ret += d;
    }
    ret += "] queue:[";

    n = 0;
    for(auto &q : b.queue)
    {
        if (n++)
            ret += ", ";
        ret += std::to_string(q);
    }
    ret += "]";

    return ret + " }";
}


binding
make_binding(const char *value)
{
    binding ret { {}, {} };

    auto vec = split(value, '.');

    ret.dev = split(vec[0].c_str(), ':');

    if (vec.size() > 1)
    {
        unsigned int n = 1;
        for(; n != vec.size(); n++)
        {
            ret.queue.push_back(std::atoi(vec[n].c_str()));
        }
    }

    return ret;
}



using namespace net;
using namespace pfq_lang;


namespace vt100
{
    const char * const CLEAR = "\E[2J";
    const char * const EDOWN = "\E[J";
    const char * const DOWN  = "\E[1B";
    const char * const HOME  = "\E[H";
    const char * const ELINE = "\E[K";
    const char * const BOLD  = "\E[1m";
    const char * const RESET = "\E[0m";
    const char * const BLUE  = "\E[1;34m";
    const char * const RED   = "\E[31m";
}


namespace test
{
    struct context
    {
        context(int id, const binding &b)
        : m_id(id)
        , m_bind(b)
        , m_pfq()
        , m_sent(std::unique_ptr<std::atomic_ullong>(new std::atomic_ullong(0)))
        , m_gen()
        {
            if (m_bind.dev.empty())
                throw std::runtime_error("context: device unspecified");

            if (m_bind.queue.empty())
                m_bind.queue.push_back(-1);

            for(unsigned int n = 0; n < m_bind.queue.size(); n++)
            {
                auto q = pfq(opt::len);

                q.bind_tx (m_bind.dev.at(0).c_str(), m_bind.queue[n]);

                q.enable();

                q.start_tx_thread(m_bind.queue[n]);

                m_pfq.push_back(std::move(q));

                std::cout << "thread: " << id << " -> gen "  << m_bind.dev.at(0) << "." << m_bind.queue[n] << std::endl;
            }
        }

        context(const context &) = delete;
        context& operator=(const context &) = delete;

        context(context &&) = default;
        context& operator=(context &&) = default;

        void operator()()
        {
            auto ip = reinterpret_cast<iphdr *>(packet + 14);

            if (opt::async)
            {
                for(;;)
                {
                    for(unsigned int n = 0; n < m_pfq.size(); n++)
                    {
                        if (opt::rand_ip)
                        {
                            ip->saddr = m_gen();
                            ip->daddr = m_gen();
                        }

                        if (m_pfq[n].send_async(net::const_buffer(reinterpret_cast<const char *>(packet), opt::len)))
                            m_sent->fetch_add(1, std::memory_order_relaxed);
                    }
                }
            }
            else
            {
                for(;;)
                {
                    for(unsigned int n = 0; n < m_pfq.size(); n++)
                    {
                        if (opt::rand_ip)
                        {
                            ip->saddr = m_gen();
                            ip->daddr = m_gen();
                        }

                        if (m_pfq[n].send(net::const_buffer(reinterpret_cast<const char *>(packet), opt::len)))
                            m_sent->fetch_add(1, std::memory_order_relaxed);
                    }
                }
            }
        }

        pfq_stats
        stats() const
        {
            pfq_stats ret = {0, 0, 0 ,0 ,0};

            for(auto & q : m_pfq)
            {
                ret +=  q.stats();
            }

            return ret;
        }

    private:
        int m_id;
        binding m_bind;

        std::vector<pfq> m_pfq;

        std::unique_ptr<std::atomic_ullong> m_sent;

        std::mt19937 m_gen;
    };

}


unsigned int hardware_concurrency()
{
    auto proc = []() {
        std::ifstream cpuinfo("/proc/cpuinfo");
        return std::count(std::istream_iterator<std::string>(cpuinfo),
                          std::istream_iterator<std::string>(),
                          std::string("processor"));
    };

    return std::thread::hardware_concurrency() ? : proc();
}


void usage(const char *name)
{
    throw std::runtime_error(std::string("usage: ") + name + " [-h|--help] [-r|--rand-ip] [-a|--async] [-l|--len] T1 T2... \n\t| T = dev[.queue.queue..]");
}


int
main(int argc, char *argv[])
try
{
    if (argc < 2)
        usage(argv[0]);

    std::vector<std::thread> vt;
    std::vector<test::context> ctx;
    std::vector<binding> thread_binding;

    for(int i = 1; i < argc; ++i)
    {
        if ( strcmp(argv[i], "-l") == 0 ||
             strcmp(argv[i], "--len") == 0) {
            i++;
            if (i == argc)
            {
                throw std::runtime_error("len missing");
            }

            opt::len = std::atoi(argv[i]);
            continue;
        }

        if ( strcmp(argv[i], "-a") == 0 ||
             strcmp(argv[i], "--async") == 0) {

            opt::async = true;
            continue;
        }

        if ( strcmp(argv[i], "-r") == 0 ||
             strcmp(argv[i], "--rand-ip") == 0) {

            opt::rand_ip = true;
            continue;
        }

        if ( strcmp(argv[i], "-h") == 0 ||
             strcmp(argv[i], "--help") == 0)
            usage(argv[0]);

        thread_binding.push_back(make_binding(argv[i]));
    }

    std::cout << "async: "  << std::boolalpha << opt::async << std::endl;
    std::cout << "len  : "  << opt::len << std::endl;

    packet = make_packet(opt::len);;

    // create thread context:
    //
    for(unsigned int i = 0; i < thread_binding.size(); ++i)
    {
        ctx.push_back(test::context(i, thread_binding[i]));
    }

    // create threads:

    int i = 0;
    std::for_each(thread_binding.begin(), thread_binding.end(), [&](binding &b) {

                  std::thread t(std::ref(ctx[i++]));

                  std::cout << "thread: " << show_binding(b) << std::endl;

                  vt.push_back(std::move(t));
    });

    pfq_stats cur, prec = {0, 0, 0, 0, 0};

    std::cout << "------------ gen started ------------\n";

    auto begin = std::chrono::system_clock::now();

    for(int y=0; true; y++)
    {
        std::this_thread::sleep_for(std::chrono::seconds(1));

        cur = {0,0,0,0,0};
        std::for_each(ctx.begin(), ctx.end(), [&](const test::context &c)
        {
            cur += c.stats();
        });

        auto end = std::chrono::system_clock::now();
        auto delta = std::chrono::duration_cast<std::chrono::microseconds>(end-begin).count();

        std::cout << "stats: { " << cur << " } -> sent: " << vt100::BOLD << ((cur.sent-prec.sent)*1000000)/delta << vt100::RESET << " pkt/sec - "
                  << "disc: " << vt100::BOLD << ((cur.disc-prec.disc)*1000000)/delta << vt100::RESET << " pkt/sec" << std::endl;

        prec = cur, begin = end;
    }

    std::for_each(vt.begin(), vt.end(), std::mem_fn(&std::thread::join));

    return 0;
}
catch(std::exception &e)
{
    std::cerr << e.what() << std::endl;
}
