/***************************************************************
 *
 * (C) 2014 - Nicola Bonelli <nicola@pfq.io>
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

#include <pfq/pfq.hpp>
#include <pfq/lang/lang.hpp>
#include <pfq/lang/default.hpp>
#include <pfq/lang/experimental.hpp>

#include <affinity.hpp>
#include <binding.hpp>
#include <vt100.hpp>

using namespace pfq;
using namespace pfq::lang;
using namespace pfq::lang::experimental;


///////////////////////////////////////////

auto computation = unit;

///////////////////////////////////////////


namespace opt
{
    std::string function;

    size_t caplen = 64;
    size_t slots  = 131072;
    bool   flow   = false;
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
        " -s --slot INT                 Set slots\n"
        " -f --function FUNCTION\n"
        " -b --binding=BINDING\n\n"
        "      BINDING = " + pfq::binding_format + "\n" +
        "      FUNCTION = fun[ >-> fun >-> fun]"
    );
}


int
main(int argc, char *argv[])
try
{
    if (argc < 2)
        usage(argv[0]);

    pfq::binding bind;

    for(int i = 1; i < argc; ++i)
    {
        if ( any_strcmp(argv[i], "-f", "--fun") )
        {
            if (++i == argc)
                throw std::runtime_error("group function missing");

            opt::function.assign(argv[i]);
            continue;
        }

        if ( any_strcmp(argv[i], "-c", "--caplen") )
        {
            if (++i == argc)
                throw std::runtime_error("caplen missing");

            opt::caplen = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if ( any_strcmp(argv[i], "-s", "--slots") )
        {
            if (++i == argc)
                throw std::runtime_error("slots missing");

            opt::slots = static_cast<size_t>(std::atoi(argv[i]));
            continue;
        }

        if ( any_strcmp(argv[i], "-w", "--flow") )
        {
            opt::flow = true;
            continue;
        }

        if ( any_strcmp(argv[i], "-b", "--binding") )
        {
            if (++i == argc)
                throw std::runtime_error("descriptor missing");

            bind = make_binding(argv[i]);
            continue;
        }

        if ( any_strcmp(argv[i], "-h", "-?", "--help") )
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

    //////////////////////////////////////////////////////////////////////

    pfq::socket q(group_policy::undefined, opt::caplen, opt::slots);

    if (bind.gid == -1)
        bind.gid = 1;

    q.join_group(bind.gid, group_policy::shared);

    for(auto &d : bind.dev)
    {
        if (bind.queue.empty())
        {
            q.bind_group(bind.gid, d.c_str(), -1);
            std::cout << "+ bind to " << d << "@" << -1 << std::endl;
        }
        else
            for(auto bq : bind.queue)
            {
                std::cout << "+ bind to " << d << "@" << bq << std::endl;
                q.bind_group(bind.gid, d.c_str(), bq);
            }
    }

    if (!opt::function.empty())
    {
        std::cout << "fun: " << opt::function << std::endl;
        q.set_group_computation(bind.gid, opt::function);
    }
    else
    {
        std::cout << "fun: " << pretty(computation) << std::endl;
        q.set_group_computation(bind.gid, computation);
    }

    q.timestamp_enable(false);

    q.enable();

    //////////////////////////////////////////////////////////////////////

    std::atomic<unsigned long> read { 0 };

    std::thread([&]() {

        std::cout << "----------- capture started ------------\n";

        unsigned long long sum, old = 0;
        pfq_stats sum_stats, old_stats = {0,0,0,0,0,0,0};

        auto begin = std::chrono::system_clock::now();

        for(int y=0;; y++)
        {
            std::this_thread::sleep_for(std::chrono::seconds(1));

            sum = 0;
            sum_stats = {0,0,0,0,0,0,0};

            sum       += read;
            sum_stats += q.stats();

            std::cout << "recv: " << sum_stats.recv << std::endl;
            std::cout << "lost: " << sum_stats.lost << std::endl;
            std::cout << "drop: " << sum_stats.drop << std::endl;

            auto end = std::chrono::system_clock::now();

            std::cout << "capture: " << vt100::BOLD <<
                (static_cast<int64_t>(sum-old)*1000000)/std::chrono::duration_cast<std::chrono::microseconds>(end-begin).count()
            << vt100::RESET << " pkt/sec" << std::endl;

            old = sum, begin = end;
            old_stats = sum_stats;
        }

    }).detach();

    for(;;)
    {
        auto many = q.read(50000);

        read += many.size();
    }
}
catch(std::exception &e)
{
    std::cerr << e.what() << std::endl;
}
