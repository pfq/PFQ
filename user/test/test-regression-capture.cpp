/***************************************************************
 *
 * (C) 2011 - Giacomo Volpi <volpozzo@gmail.com>
 *            Nicola Bonelli <nicola.bonelli@cnit.it>
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

// ----------------------------------------
//

#include <unordered_map>
#include <utility>
#include <cstring>

#include <pcap.h>
#include <signal.h>
#include <netinet/ip.h>
#include <arpa/inet.h>
#include <net/ethernet.h>
#include <netinet/if_ether.h>
#include <linux/if_ether.h>


namespace opt {

    int sleep_microseconds;

    std::string steer_function;
    std::string pcap_file;

    size_t caplen = 64;
    size_t offset = 0;
    size_t slots  = 262144;

    int group_id  = 42;

    static const int seconds = 60;
}


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


class buffer
{
public:
    buffer(size_t caplen, const u_char* packet)
    : caplen_(caplen)
    , offset_(opt::offset)
    {
        buff_ = reinterpret_cast<char *>(malloc(caplen - offset_));
        memcpy(buff_, packet + offset_, (caplen - offset_));
    }

    ~buffer()
    {
        free(buff_);
    }

    // non copyable, not assignable
    buffer(const buffer &other) = delete;
    buffer &operator=(const buffer &other) = delete;

    // move constructor
    buffer(buffer &&other)
    : buff_(other.buff_), caplen_(other.caplen_)
    {
        other.caplen_ = 0;
        other.buff_ = nullptr;
    }

    // move assignment constructor
    buffer &operator=(buffer &&other)
    {
        free(buff_);
        buff_ = other.buff_;
        caplen_ = other.caplen_;
        other.buff_ = nullptr;
        return *this;
    }

    size_t get_caplen() const
    {
        return caplen_;
    }

    char *get_data()const
    {
        return buff_;
    }

private:
    char *buff_;
    size_t caplen_;
    size_t offset_;
};


typedef std::unordered_map<uint32_t, buffer> unmap_caplen;
unmap_caplen map_cap;

std::atomic_bool stop(false);


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

            std::for_each(m_queues.begin(), m_queues.end(),[&](int q) {
                          std::cout << "adding bind to " << d << "@" << q << std::endl;
                          m_pfq.bind_group(gid, d, q);
                          });

            if (!opt::steer_function.empty() && (m_id == 0))
            {
                m_pfq.set_group_function(gid, opt::steer_function.c_str(), 0);
            }

            m_pfq.timestamp_enable(false);

            m_pfq.enable();

            std::cout << "ctx: queue_slots: " << m_pfq.rx_slots() << " pfq_id:" << m_pfq.id() << std::endl;

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

        typedef std::unordered_map<uint32_t, std::pair<int, int>> umap_counter_type;
        umap_counter_type map_counter;


        auto umap_begin() const
        -> decltype(std::begin(map_counter))
        {
            return std::begin(map_counter);
        }

        auto umap_end() const
        -> decltype(std::end(map_counter))
        {
            return std::end(map_counter);
        }

        void parse_packet (uint16_t cap_pfq, const char *data)
        {
            struct ether_header *eth;
            struct iphdr *ip;

            eth = (struct ether_header *)(data - opt::offset);
            if (ntohs(eth->ether_type) == ETHERTYPE_IP)
            {
                ip = (struct iphdr*) (data - opt::offset + sizeof(ether_header));

                map_counter[ip->saddr].first++;

                auto it = map_cap.find(ip->saddr);

                if (it != map_cap.end())
                {
                    if(memcmp(data, it->second.get_data(), cap_pfq) == 0)
                        map_counter[ip->saddr].second++;
                }
            }
        }

        ////////////////////////////////////////////////////////////////////////////

        void operator()()
        {
            for(;;)
            {
                auto many = m_pfq.read(opt::sleep_microseconds);

                queue::iterator it = many.begin();
                queue::iterator it_e = many.end();

                for(; it != it_e; ++it)
                {

                    char *packet = static_cast<char *>(it.data());
                    parse_packet(it->caplen, packet);
                }

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
        int m_id;

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
    throw std::runtime_error(std::string("usage: ")
                             .append(name)
                             .append("[-h|--help] [-c caplen] [-f pcapfile] [-o offset] [-s slots] [-g gid ] [-b|--balance function-name] T1 T2... | T = dev:core:queue,queue..."));
}


void packet_handler(u_char *, const struct pcap_pkthdr *h, const u_char *bytes)
{
    struct ether_header *eth;
    struct iphdr *ip;
    eth = (struct ether_header *)(bytes);
    if (ntohs(eth->ether_type) == ETHERTYPE_IP)
    {
        ip = (struct iphdr*) (bytes + sizeof(ether_header));

        map_cap.insert(unmap_caplen::value_type(ip->saddr, buffer(h->caplen, bytes)));
    }
};


//////////////////////////////////////////////////////////

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
            i++;
            if (i == argc)
            {
                throw std::runtime_error("steer function missing");
            }
            opt::steer_function.assign(argv[i]);

            std::cout << "Balancing with [" << opt::steer_function << "]" << std::endl;
            continue;
        }

        if ( strcmp(argv[i], "-f") == 0 ||
             strcmp(argv[i], "--file") == 0) {
            i++;
            if (i == argc)
            {
                throw std::runtime_error("pcap filename missing");
            }
            opt::pcap_file.assign(argv[i]);

            std::cout << "Pcap file: " << opt::pcap_file << std::endl;
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

            if (opt::caplen < 30 || opt::caplen > 1525)
            {
                throw std::runtime_error("caplen < 26: can't find source ip || caplen > 1525: MTU exceeded");
            }

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

            if (opt::offset > 12)
            {
                throw std::runtime_error("offset: offset > 12 bytes");
            }

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

        if ( strcmp(argv[i], "-h") == 0 ||
             strcmp(argv[i], "--help") == 0)
            usage(argv[0]);

        vbinding.push_back(binding_parser(argv[i]));
    }

    if (opt::caplen < opt::offset )
        throw std::runtime_error ("CAPLEN < OFFSET");

    std::cout << "Caplen: " << opt::caplen << std::endl;
    std::cout << "Slots : " << opt::slots << std::endl;

    char errbuf[PCAP_ERRBUF_SIZE];

    pcap_t *handler = pcap_open_offline (opt::pcap_file.c_str(), errbuf);

    if (pcap_dispatch(handler, -1, packet_handler, nullptr) == -1)
    {
        std::cout << "pcap_dispatch error" << std::endl;
    }

    // ignore signals:
    //

    sigset_t set;
    sigfillset(&set);
    sigprocmask(SIG_BLOCK, &set, nullptr);

    std::thread sighandler([]
    {
        sigset_t set;
        int sig;
        sigfillset(&set);

        for(;;)
        {
            if(sigwait(&set, &sig) !=0)
               throw std::logic_error("sighandler");
            switch(sig)
            {
                case SIGQUIT:
                case SIGINT:
                case SIGSTOP:
                case SIGTERM:
                {
                    stop.store(true);
                    return;
                }
                default:
                std::cout << "sighandler: signal" << sig << " ignored" << std::endl;
            }
        }
    });

    sighandler.detach();

    // create threads' context:
    //

    for(unsigned int i = 0; i < vbinding.size(); ++i)
    {
        std::cout << "pushing a context: " << std::get<0>(vbinding[i]) << ' ' << std::get<1>(vbinding[i]) << std::endl;
        ctx.push_back(test::ctx(i, std::get<0>(vbinding[i]).c_str(), std::get<2>(vbinding[i])));
    }

    opt::sleep_microseconds = 40000 * ctx.size();
    std::cout << "poll timeout " << opt::sleep_microseconds << " usec" << std::endl;

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
    pfq_stats sum_stats, old_stats = {0,0,0,0,0};

    std::cout << "----------- capture started ------------\n";

    auto begin = std::chrono::system_clock::now();

    for(int y=0; y < opt::seconds; y++)
    {
        if (stop.load())
            break;

        std::this_thread::sleep_for(std::chrono::seconds(1));

        sum = 0;
        sum_stats = {0,0,0,0,0};

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

        std::cout << "capture: " << vt100::BOLD <<
        ((sum-old)*1000000)/std::chrono::duration_cast<std::chrono::microseconds>(end-begin).count()
        << vt100::RESET << " pkt/sec" << std::endl;

        old = sum, begin = end;
        old_stats = sum_stats;
    }

    // stopping threads...
    std::for_each(ctx.begin(), ctx.end(), std::mem_fn(&test::ctx::stop));

    std::for_each(vt.begin(), vt.end(), std::mem_fn(&std::thread::join));

    test::ctx::umap_counter_type map_tot;
    int thread = 1;

    std::for_each(ctx.begin(), ctx.end(), [&](const test::ctx &c)
    {
         std::cout << std::endl;
         std::cout << "\t ///// \tPKT ON THREAD #" << thread << " /////" <<std::endl;

         thread++;

         auto it = c.umap_begin();
         auto it_e = c.umap_end();

         for (; it != it_e; ++it)
         {
             char ip_addr[INET_ADDRSTRLEN];
             auto y = it->first;

             inet_ntop(AF_INET, &(y), ip_addr, sizeof(ip_addr));

             std::cout << "ip: " << ip_addr << "\tpkt_arr: "<< it->second.first << "\tpkt_match: " << it->second.second <<std::endl;

             map_tot[it->first].first  += it->second.first;
             map_tot[it->first].second += it->second.second;
         }

         std::cout << std::endl;
    });

    std::cout << std::endl;
    std::cout << "\t ////////// \t GLOBAL STATISTICS \t//////////" << std::endl;

    char ip_[INET_ADDRSTRLEN];

    auto it_tot = map_tot.begin();
    auto it_e_tot = map_tot.end();

    for (; it_tot != it_e_tot; it_tot++)
    {
        inet_ntop(AF_INET, &(it_tot->first), ip_, sizeof(ip_));

        std::cout << "ip: " << ip_ << "\tpkt_arr: " << it_tot->second.first << "\tpkt_match: " << it_tot->second.second << std::endl;
    }
    std::cout << std::endl;

    return 0;
}
catch(std::exception &e)
{
    std::cerr << e.what() << std::endl;
}
