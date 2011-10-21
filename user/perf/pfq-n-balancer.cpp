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
    if (sc == arg+strlen(arg))
        throw std::runtime_error("binding: parser error: ':' not found");

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
        : m_buffer(), m_dev(d), m_queues(q), m_stop(false), m_pfq(pfq_open), m_read()
        {
            std::for_each(m_queues.begin(), m_queues.end(),[&](int q) {
                    m_pfq.add_device(m_pfq.ifindex(d), q);
                });

            m_pfq.enable();
            m_pfq.tstamp(true);
            m_pfq.load_balance(true);

            m_buffer = new char[m_pfq.queue_size()];

            std::cout << "cxt: queue_size: " << m_pfq.queue_size() << std::endl;
        }
        
        ctx(const ctx &) = delete;
        ctx& operator=(const ctx &) = delete;

        ctx(ctx && other)
        : m_buffer(other.m_buffer), m_dev(other.m_dev), m_queues(other.m_queues), m_stop(other.m_stop.load()), 
          m_pfq(std::move(other.m_pfq)), m_read()
        {
            other.m_buffer = 0;
        }

        ctx& operator=(ctx &&other)
        {
            m_dev = other.m_dev;
            m_queues = other.m_queues;
            m_stop.store(other.m_stop.load());
            m_pfq = std::move(other.m_pfq);
            m_buffer = other.m_buffer;

            other.m_pfq = pfq();
            other.m_buffer = 0;
            return *this;
        }

        void operator()() 
        {
            for(;;)
            {
                // batch many = m_pfq.recv(mutable_buffer(m_buffer, m_pfq.queue_size()),sleep_microseconds);
                // auto many = m_pfq.recv(mutable_buffer(m_buffer, m_pfq.queue_size()), sleep_microseconds);
                
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

        // unsigned long long
        // counter() const
        // {
        //     return static_cast<volatile long long int>(m_counter); // read of 64bit is atomic only on 64bits arch. 
        // }
        
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
        char *m_buffer;

        const char *m_dev;
        std::vector<int> m_queues;

        std::atomic_bool m_stop;
        
        pfq m_pfq;        

        unsigned long long m_read;
        size_t m_batch;

    } __attribute__((aligned(64)));
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


int
main(int argc, char *argv[])
{
    if (argc < 2)
        throw std::runtime_error(std::string("usage: ").append(argv[0]).append(" T1 T2... | T = dev:core:queue,queue..."));
    
    std::vector<std::thread> vt;
    std::vector<test::ctx> ctx;

    std::vector<binding_type> vbinding;
    
    // load vbinding vector:
    for(int i = 1; i < argc; ++i)
    {
        vbinding.push_back(binding_parser(argv[i]));
    }
    
    // create threads' context:
    for(int i = 1; i < argc; ++i)
    {
        std::cout << "pushing a context: " << std::get<0>(vbinding[i-1]) << ' ' << std::get<1>(vbinding[i-1]) << std::endl;
        ctx.push_back(test::ctx(std::get<0>(vbinding[i-1]).c_str(), std::get<2>(vbinding[i-1])));        
    }

    sleep_microseconds = 20000 * ctx.size();
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
 
