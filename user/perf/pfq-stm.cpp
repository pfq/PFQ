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

#include <pfq.hpp>

#include <netinet/ip.h>
#include <netinet/udp.h>

namespace opt {

    int sleep_microseconds;
    std::string function;

    size_t caplen = 64;
    size_t offset = 0;
    size_t slots  = 262144;

    int group_id  = 42;

    bool flow = false;

    static const int seconds = 600;
}


typedef std::tuple<std::string, int, std::vector<int>> binding_type;

using namespace net;

typedef std::tuple<uint32_t, uint32_t, uint16_t, uint16_t> Tuple;


struct HashTuple {

    uint32_t operator()(Tuple const &t) const
    {

        return std::get<0>(t) ^ std::get<1>(t) ^
               std::get<2>(t) ^ (std::get<3>(t) << 16);
    }

};


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
        ctx(int id, const char *d, const std::vector<int> & q)
        : m_id(id), m_dev(d), m_queues(q), m_stop(false), m_pfq(group_policy::undefined, opt::caplen, opt::offset, opt::slots), m_read()
        {
            int gid = opt::group_id != -1 ? opt::group_id : id;

            m_pfq.join_group(gid, group_policy::shared);
            
            if (id == 0) 
            {
                struct stm_context { int value; } x = {0};
                
                m_pfq.set_group_function_context(gid, x);
            }

            std::for_each(m_queues.begin(), m_queues.end(),[&](int q) {
                          std::cout << "adding bind to " << d << "@" << q << std::endl;       
                    m_pfq.bind_group(gid, d, q);
                });

            if (!opt::function.empty() && (m_id == 0))
            {
                m_pfq.set_group_function(gid, opt::function.c_str(), 0);
            }   

            m_pfq.timestamp_enabled(false);
            
            m_pfq.enable();

            std::cout << "ctx: queue_slots: " << m_pfq.slots() << " pfq_id:" << m_pfq.id() << std::endl;
        }
        
        ctx(const ctx &) = delete;
        ctx& operator=(const ctx &) = delete;

        ctx(ctx && other)
        : m_id(other.m_id), m_dev(other.m_dev), m_queues(other.m_queues), m_stop(other.m_stop.load()), 
          m_pfq(std::move(other.m_pfq)), m_read()
        {
        }

        ctx& operator=(ctx &&other)
        {
            m_id = other.m_id;
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
                auto many = m_pfq.read(opt::sleep_microseconds);

                m_read += many.size();

                m_batch = std::max(m_batch, many.size());

                if (m_stop.load(std::memory_order_relaxed))
                    return;

                if (opt::flow) 
                {
                    auto it = many.begin();
                    auto it_e = many.end();
                    for(; it != it_e; ++it)
                    {
                        while(!it.ready())
                            std::this_thread::yield();

                        iphdr  *  ip = static_cast<iphdr *> (it.data());
                        if (ip->protocol == IPPROTO_TCP ||
                            ip->protocol == IPPROTO_UDP)
                        {
                            udphdr * udp = reinterpret_cast<udphdr *>(static_cast<char *>(it.data()) + (ip->ihl<<2));
                     
                            if (m_set.insert(std::make_tuple(ip->saddr, ip->daddr, udp->source, udp->dest)).second)
                                m_flow++;
                        }
                    }
                }
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

        const char *m_dev;
        std::vector<int> m_queues;

        std::atomic_bool m_stop;
        
        pfq m_pfq;        

        unsigned long long m_read;
        size_t m_batch;

        std::unordered_set<std::tuple<uint32_t, uint32_t, uint16_t, uint16_t>, HashTuple> m_set;

        unsigned long m_flow;

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
    throw std::runtime_error(std::string("usage: ")
               .append(name)
               .append("[-h|--help] [-c caplen] [-o offset] [-f | --flow] [-s slots] [-g gid ] [-x|--steer function-name] T1 T2... \n\t| T = dev:core:queue,queue..."));
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
        if ( strcmp(argv[i], "-x") == 0 ||
             strcmp(argv[i], "--steer") == 0) {
            i++;
            if (i == argc)
            {
                throw std::runtime_error("function missing");
            }
            opt::function.assign(argv[i]);

            std::cout << "balancing with [" << opt::function << "]" << std::endl;
            continue;
        }

        if ( strcmp(argv[i], "-c") == 0 ||
             strcmp(argv[i], "--caplen") == 0) {
            i++;
            if (i == argc)
            {
                throw std::runtime_error("caplen missing");
            }

            opt::caplen = std::atoi(argv[i]);
            continue;
        }

        if ( strcmp(argv[i], "-o") == 0 ||
             strcmp(argv[i], "--offset") == 0) {
            i++;
            if (i == argc)
            {
                throw std::runtime_error("offset missing");
            }

            opt::offset = std::atoi(argv[i]);
            continue;
        }

        if ( strcmp(argv[i], "-s") == 0 ||
             strcmp(argv[i], "--slots") == 0) {
            i++;
            if (i == argc)
            {
                throw std::runtime_error("slots missing");
            }

            opt::slots = std::atoi(argv[i]);
            continue;
        }

        if ( strcmp(argv[i], "-g") == 0 ||
             strcmp(argv[i], "--gid") == 0) {
            i++;
            if (i == argc)
            {
                throw std::runtime_error("group_id missing");
            }

            if (strcmp(argv[i], "any") == 0)
                opt::group_id = -1;
            else
                opt::group_id = std::atoi(argv[i]);

            continue;
        }

        if ( strcmp(argv[i], "-f") == 0 ||
             strcmp(argv[i], "--flow") == 0)
        {
            opt::flow = true;
            continue;
        }

        if ( strcmp(argv[i], "-h") == 0 ||
             strcmp(argv[i], "--help") == 0)
            usage(argv[0]);

        vbinding.push_back(binding_parser(argv[i]));
    }
    
    std::cout << "caplen: " << opt::caplen << std::endl;
    std::cout << "slots : " << opt::slots << std::endl;
    
    if (opt::slots < 1024)
    {
        std::cout << "too few slots may affet the performance!" << std::endl;
        _Exit(0); 
    }

    if (opt::flow) {
        std::cout << "forcing offset to 14 bytes..." << std::endl;
        opt::offset = 14;
    }
    
    std::cout << "offset: " << opt::offset << std::endl;
    
    // create threads' context:
    //
    for(unsigned int i = 0; i < vbinding.size(); ++i)
    {
        auto & qs = std::get<2>(vbinding[i]);
        if (qs.empty())
            qs.push_back(-1);

        std::cout << "pushing a context: " << std::get<0>(vbinding[i]) << ' ' << std::get<1>(vbinding[i]) << std::endl;
        
        ctx.push_back(test::ctx(i, std::get<0>(vbinding[i]).c_str(), std::get<2>(vbinding[i])));        
    }

    opt::sleep_microseconds = 50000 * ctx.size();
    std::cout << "poll timeout " << opt::sleep_microseconds << " usec" << std::endl;

    // create threads:

    int i = 0;
    std::for_each(vbinding.begin(), vbinding.end(), [&](binding_type &b) {

                  std::thread t(std::ref(ctx[i++]));

                  std::cout << "thread on core " << std::get<1>(b) << " -> queues [";

                  std::copy(std::get<2>(b).begin(), std::get<2>(b).end(),
                            std::ostream_iterator<int>(std::cout, " "));
                  std::cout << "]\n";

                  extra::set_affinity(t, std::get<1>(b));

                  vt.push_back(std::move(t));
    });

    unsigned long long sum, flow, old = 0;
    pfq_stats sum_stats, old_stats = {0,0,0};

    std::cout << "----------- capture started ------------\n";

    auto begin = std::chrono::system_clock::now();

    for(int y=0; y < opt::seconds; y++)
    {
        std::this_thread::sleep_for(std::chrono::seconds(1));
        
        sum = 0;
        flow = 0;
        sum_stats = {0,0,0};

        std::for_each(ctx.begin(), ctx.end(), [&](const test::ctx &c) {
                        sum += c.read();
                        flow += c.flow();
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

        std::cout << "capture: " << vt100::BOLD << 
                ((sum-old)*1000000)/std::chrono::duration_cast<std::chrono::microseconds>(end-begin).count() 
                    << vt100::RESET << " pkt/sec"; 
        
        if (flow) {
            std::cout << " flow: " << flow;    
        }

        std::cout << std::endl; 

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
