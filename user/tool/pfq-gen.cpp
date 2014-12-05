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
#include <random>

#include <pfq/pfq.hpp>
#include <pfq/util.hpp>

#include <netinet/ip.h>
#include <netinet/udp.h>

#include <vt100.hpp>


using namespace pfq;


struct binding;


char *make_packet(size_t n)
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

    auto p = new char[n];

    memcpy(p, ping, std::min(n, sizeof(ping)));

    for(auto i = sizeof(ping); i < n; i++)
    {
        p[i] = static_cast<char>(0x38 + i - sizeof(ping));
    }

    return p;
}


namespace opt
{
    size_t batch   = 64;
    size_t len     = 64;
    size_t slots   = 4096;
    bool   async   = false;
    bool   rand_ip = false;

    char *packet = nullptr;
}


struct binding
{
    std::string                 dev;
    std::vector<int>            queue;
};


std::string
show_binding(const binding &b)
{
    std::string ret = "binding:{ dev:" + b.dev + " queues:";

    for(size_t n = 0; n < b.queue.size(); ++n)
    {
        ret += std::to_string(b.queue[n]) + ' ';
    }

    return ret + "}";
}


binding
make_binding(const char *value)
{
    binding ret { "", {} };

    auto vec = split(value, ":");
    ret.dev = vec.at(0);

    if (vec.size() > 1)
    {
        auto queues = split(vec.at(1), ".");
        for(auto &q : queues)
        {
            ret.queue.push_back(std::atoi(q.c_str()));
        }

    } else
    {
        ret.queue.push_back(any_queue);
    }

    return ret;
}


namespace thread
{
    struct context
    {
        context(int id, const binding &b)
        : m_id(id)
        , m_bind(b)
        , m_pfq()
        , m_sent(std::unique_ptr<std::atomic_ullong>(new std::atomic_ullong(0)))
        , m_fail(std::unique_ptr<std::atomic_ullong>(new std::atomic_ullong(0)))
        , m_gen()
        {
            if (m_bind.dev.empty())
                throw std::runtime_error("context: device unspecified");

            if (m_bind.queue.empty())
                m_bind.queue.push_back(-1);

            auto q = pfq::socket(param::list, param::maxlen{opt::len},
                                              param::tx_slots{opt::slots});

            for(unsigned int n = 0; n < m_bind.queue.size(); n++)
            {
                q.bind_tx (m_bind.dev.c_str(), m_bind.queue[n], opt::async ? n : -1);
                std::cout << "thread: " << id << " -> gen "  << m_bind.dev << ":" << m_bind.queue[n] << std::endl;
            }

            q.enable();
            m_pfq = std::move(q);
        }

        context(const context &) = delete;
        context& operator=(const context &) = delete;

        context(context &&) = default;
        context& operator=(context &&) = default;

        void operator()()
        {
            auto ip = reinterpret_cast<iphdr *>(opt::packet + 14);

            for(;;)
            {
                if (opt::rand_ip)
                {
                    ip->saddr = static_cast<uint32_t>(m_gen());
                    ip->daddr = static_cast<uint32_t>(m_gen());
                }

                if (m_pfq.send_async(pfq::const_buffer(reinterpret_cast<const char *>(opt::packet), opt::len), opt::batch))
                    m_sent->fetch_add(1, std::memory_order_relaxed);
                else
                    m_fail->fetch_add(1, std::memory_order_relaxed);
            }
        }

        std::tuple<pfq_stats, uint64_t, uint64_t>
        stats() const
        {
            pfq_stats ret = {0,0,0,0,0,0,0};

            ret += m_pfq.stats();

            return std::make_tuple(ret, m_sent->load(std::memory_order_relaxed),
                                        m_fail->load(std::memory_order_relaxed));
        }

    private:
        int m_id;
        binding m_bind;

        pfq::socket m_pfq;

        std::unique_ptr<std::atomic_ullong> m_sent;
        std::unique_ptr<std::atomic_ullong> m_fail;

        std::mt19937 m_gen;
    };

}


void usage(const char *name)
{
    throw std::runtime_error(std::string("usage: ") + name +
        " [-h|--help] [-r|--rand-ip] [-a|--async] [-s|--queuel-slots N] "
        "[-b|--batch-sync N] [-l|--len N] T1 T2... \n\t| T = dev[:queue[.queue...]]");
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
        if ( strcmp(argv[i], "-b") == 0 ||
             strcmp(argv[i], "--batch") == 0) {
            i++;
            if (i == argc)
            {
                throw std::runtime_error("batch length missing");
            }

            opt::batch = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if ( strcmp(argv[i], "-l") == 0 ||
             strcmp(argv[i], "--len") == 0) {
            i++;
            if (i == argc)
            {
                throw std::runtime_error("length missing");
            }

            opt::len = static_cast<size_t>(std::atoi(argv[i]));
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

        if ( strcmp(argv[i], "-?") == 0 ||
             strcmp(argv[i], "-h") == 0 ||
             strcmp(argv[i], "--help") == 0)
            usage(argv[0]);

        thread_binding.push_back(make_binding(argv[i]));
    }

    std::cout << "async: "  << std::boolalpha << opt::async << std::endl;
    std::cout << "len  : "  << opt::len << std::endl;

    opt::packet = make_packet(opt::len);;

    // create thread context:
    //
    for(unsigned int i = 0; i < thread_binding.size(); ++i)
    {
        ctx.push_back(thread::context(static_cast<int>(i), thread_binding[i]));
    }

    // create threads:

    size_t i = 0;
    std::for_each(thread_binding.begin(), thread_binding.end(), [&](binding &b) {

                  std::thread t(std::ref(ctx[i++]));

                  std::cout << "thread: " << show_binding(b) << std::endl;

                  vt.push_back(std::move(t));
    });

    pfq_stats cur, prec = {0,0,0,0,0,0,0};
    uint64_t sent, sent_ = 0;
    uint64_t fail, fail_ = 0;

    std::cout << "------------ gen started ------------\n";

    auto begin = std::chrono::system_clock::now();

    for(int y=0; true; y++)
    {
        std::this_thread::sleep_for(std::chrono::seconds(1));

        cur = {0,0,0,0,0,0,0};
        sent = 0;
        fail = 0;

        std::for_each(ctx.begin(), ctx.end(), [&](const thread::context &c)
        {
            auto p = c.stats();

            cur  += std::get<0>(p);
            sent += std::get<1>(p);
            fail += std::get<2>(p);
        });

        auto end   = std::chrono::system_clock::now();
        auto delta = std::chrono::duration_cast<std::chrono::microseconds>(end-begin).count();

        std::cout << "stats: { " << cur << " } -> user: { "
                  << vt100::BOLD << (static_cast<int64_t>(sent-sent_)*1000000)/delta << ' ' <<
                                    (static_cast<int64_t>(fail-fail_)*1000000)/delta << vt100::RESET << " } - "
                  << "sent: " << vt100::BOLD << (static_cast<int64_t>(cur.sent-prec.sent)*1000000)/delta << vt100::RESET << " pkt/sec - "
                  << "disc: " << vt100::BOLD << (static_cast<int64_t>(cur.disc-prec.disc)*1000000)/delta << vt100::RESET << " pkt/sec" << std::endl;

        prec = cur, begin = end;
        sent_ = sent;
        fail_ = fail;
    }

}
catch(std::exception &e)
{
    std::cerr << e.what() << std::endl;
}
