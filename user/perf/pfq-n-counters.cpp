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

#include <pfq.hpp>

int sleep_microseconds;

bool enable_balance = false;

static const int SECONDS = 600;

typedef std::tuple<std::string, int, std::vector<int>> binding_type;

using namespace net;

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


binding_type 
binding_parser(const char *arg)
{
    int core, q; char sep;
    std::vector<int> queues;

    auto sc = std::find(arg, arg+strlen(arg), ':');
    if (sc == arg + strlen(arg)) {
        std::string err("'");
        err.append(arg)
           .append("' option error: ':' not found");
        throw std::runtime_error(err);
    }

    std::string dev(arg, sc);

    std::istringstream i(std::string(sc+1, arg+strlen(arg)));  

    if(!(i >> core))
        throw std::runtime_error("arg: parse error");

    while((i >> sep >> q))
        queues.push_back(q);
    
    return std::make_tuple(dev, core, queues);
}

namespace test
{
    struct ctx
    {
        ctx(const char *d, const std::vector<int> & q)
        : m_dev(d), m_queues(q), m_stop(false), m_pfq(pfq_open), m_read()
        {
            std::for_each(m_queues.begin(), m_queues.end(),[&](int q) {
                          std::cout << "setting dev: " << d << "@" << q << std::endl;       
                    m_pfq.add_device(d, q);
                });

            m_pfq.load_balance(enable_balance);
            m_pfq.enable();
            m_pfq.tstamp(false);

            std::cout << "cxt: queue_size: " << m_pfq.queue_size() << " pfq_id:" << m_pfq.get_id() << std::endl;
        }
        
        ctx(const ctx &) = delete;
        ctx& operator=(const ctx &) = delete;

        ctx(ctx && other)
        : m_dev(other.m_dev), m_queues(other.m_queues), m_stop(other.m_stop.load()), 
          m_pfq(std::move(other.m_pfq)), m_read()
        {
        }

        ctx& operator=(ctx &&other)
        {
            m_dev = other.m_dev;
            m_queues = other.m_queues;
            m_stop.store(other.m_stop.load());
            m_pfq = std::move(other.m_pfq);

            other.m_pfq = pfq();
            return *this;
        }

        void operator()() 
        {
            for(;;)
            {
                auto many = m_pfq.read(sleep_microseconds);

                m_read += many.size();

                m_batch = std::max(m_batch, many.size());

                if (m_stop.load(std::memory_order_relaxed))
                    return;
            }
        }

        void stop()
        {
            m_stop.store(true, std::memory_order_release);
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

        size_t 
        batch() const
        {
            return m_batch;
        }

    private:
        const char *m_dev;
        std::vector<int> m_queues;

        std::atomic_bool m_stop;
        
        pfq m_pfq;        

        unsigned long long m_read;
        size_t m_batch;

    } __attribute__((aligned(128)));
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
    throw std::runtime_error(std::string("usage: ").append(name).append("[-h|--help] [-b|--balance] T1 T2... | T = dev:core:queue,queue..."));
}


int
main(int argc, char *argv[])
try
{
    if (argc < 2)
        usage(argv[0]);

    std::vector<std::thread> vt;
    std::vector<test::ctx> ctx;

    std::vector<binding_type> vbinding;

    // load vbinding vector:
    for(int i = 1; i < argc; ++i)
    {
        if ( strcmp(argv[i], "-b") == 0 ||
             strcmp(argv[i], "--balance") == 0) {
            std::cout << "Balancing: ON" << std::endl;
            enable_balance = true;
            continue;
        }

        if ( strcmp(argv[i], "-h") == 0 ||
             strcmp(argv[i], "--help") == 0)
            usage(argv[0]);

        vbinding.push_back(binding_parser(argv[i]));
    }
        
    // create threads' context:
    for(int i = 0; i < vbinding.size(); ++i)
    {
        std::cout << "pushing a context: " << std::get<0>(vbinding[i]) << ' ' << std::get<1>(vbinding[i]) << std::endl;
        ctx.push_back(test::ctx(std::get<0>(vbinding[i]).c_str(), std::get<2>(vbinding[i])));        
    }

    sleep_microseconds = 30000 * ctx.size();
    std::cout << "poll timeout " << sleep_microseconds << " usec" << std::endl;

    // create threads:

    int i = 0;
    std::for_each(vbinding.begin(), vbinding.end(), [&](const binding_type &b) {
                  std::thread t(std::ref(ctx[i++]));
                  std::cout << "thread on core " << std::get<1>(b) << " -> queues [";

                  std::copy(std::get<2>(b).begin(), std::get<2>(b).end(),
                            std::ostream_iterator<int>(std::cout, " "));
                  std::cout << "]\n";

                  extra::set_affinity(t, std::get<1>(b));
                  vt.push_back(std::move(t));
                  });

    unsigned long long sum, old = 0;
    pfq_stats sum_stats, old_stats = {0,0,0};

    std::cout << "----------- capture started ------------\n";

    auto begin = std::chrono::system_clock::now();
    for(int y=0; y < SECONDS; y++)
    {
        std::this_thread::sleep_for(std::chrono::seconds(1));
        
        sum = 0;
        sum_stats = {0,0,0};

        std::for_each(ctx.begin(), ctx.end(), [&](const test::ctx &c) {
                        sum += c.read();
                        sum_stats += c.stats();
                      });
    
        std::cout << "recv: ";
        std::for_each(ctx.begin(), ctx.end(), [&](const test::ctx &c) {
            std::cout << c.stats().recv << ' ';
        });
        std::cout << " -> " << sum_stats.recv << std::endl;

        std::cout << "lost: ";
        std::for_each(ctx.begin(), ctx.end(), [&](const test::ctx &c) {
            std::cout << c.stats().lost << ' ';
        });
        std::cout << " -> " << sum_stats.lost << std::endl;

        std::cout << "drop: ";
        std::for_each(ctx.begin(), ctx.end(), [&](const test::ctx &c) {
            std::cout << c.stats().drop << ' ';
        });
        std::cout << " -> " << sum_stats.drop << std::endl;

        std::cout << "max_batch: ";
        std::for_each(ctx.begin(), ctx.end(), [&](const test::ctx &c) {
            std::cout << c.batch() << ' ';
        });
        std::cout << std::endl;

        auto end = std::chrono::system_clock::now();

        // std::cout << "capture: " << 
        //     1000000 * (static_cast<long double>(sum-old)/
        //     static_cast<long double>(static_cast<std::chrono::microseconds>(end-begin).count())) << " pkt/sec"  << std::endl;   
        
        std::cout << "capture: " << vt100::BOLD << (sum-old) << vt100::RESET << " pkt/sec" << std::endl; 

        old = sum, begin = end;
        old_stats = sum_stats;
    }

    std::for_each(ctx.begin(), ctx.end(), std::mem_fn(&test::ctx::stop));
    std::for_each(vt.begin(), vt.end(), std::mem_fn(&std::thread::join));

    return 0;
}
catch(std::exception &e)
{
    std::cerr << e.what() << std::endl;
}
