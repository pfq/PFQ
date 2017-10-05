/***************************************************************
 *
 * (C) 2011-16 - Nicola Bonelli <nicola@pfq.io>
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
#include <atomic>
#include <csignal>

#include <pfq/pfq.hpp>
#include <pfq/lang/lang.hpp>
#include <pfq/lang/default.hpp>
#include <pfq/lang/experimental.hpp>

#include <more/affinity.hpp>
#include <more/binding.hpp>
#include <more/vt100.hpp>
#include <more/pretty.hpp>

using namespace more;
using namespace pfq;
using namespace pfq::lang;
using namespace pfq::lang::experimental;


struct Bridge
{
    Bridge(bool k, size_t c, size_t q, std::string f, std::string t)
    : kernel(k)
    , core(c)
    , queue(q)
    , from(std::move(f))
    , to(std::move(t))
    { }

    bool        kernel;
    size_t      core;
    int         queue;
    std::string from;
    std::string to;
};


namespace opt
{
    bool fast_forward = false;
    size_t caplen  = 64;
    size_t slots   = 8192;
    std::atomic_bool stop;

    long timeout_ms = 1000000;
}



void usage(std::string name)
{
    throw std::runtime_error
    (
        "usage: " + std::move(name) + " [OPTIONS]\n\n"
        " -c --caplen INT                       Set caplen\n"
        " -s --slot INT                         Set slots\n"
        " -f --forward core queue Dev1 Dev2     User-space bridge: Dev1 -> Dev2\n"
        "    --fast                             Enable fast-forward...\n"
        " -b --bridge DEV1 DEV2                 Kernel bridge: Dev1 -> Dev2\n"
        " -h --help                             Display this help\n"
    );
}


void make_user_bridge(int gid, Bridge const &b)
{
    std::cout << "User-space bridge: group " << gid << " :: => bridge [" << b.from << " -> " << b.to << "]" << std::endl;

    std::thread t([=] {

        pfq::socket in (group_policy::undefined, opt::caplen, opt::slots, opt::slots);
        pfq::socket out(group_policy::undefined, opt::caplen, opt::slots, opt::slots);

        in.join_group(gid);
        in.bind_group(gid, b.from.c_str(), b.queue);

        out.bind_tx(b.to.c_str(), b.queue);

        in.enable();
        out.enable();

        for(;;)
        {
            auto many = in.read(opt::timeout_ms);

            if (many.size())
            {
                auto pkt = many.begin();
                for(; pkt != many.end(); ++pkt)
                {
                    while (!pkt.ready())
                        std::this_thread::yield();

                    auto h = *pkt;
                    const unsigned char *buff = static_cast<unsigned char *>(pkt.data());

                    out.send(pfq::const_buffer(reinterpret_cast<const char *>(buff), h.len), b.queue);
                }

                out.sync_queue(0);
            }

            if (opt::stop.load(std::memory_order_relaxed))
                    break;
        }

    });

    more::set_affinity(t, b.core);

    t.detach();
}


void make_lang_bridge(pfq::socket &q, int gid, Bridge const &b)
{
    std::cout << "In-kernel bridge: group " << gid << " :: => bridge [" << b.from << " -> " << b.to << "]" << std::endl;

    //
    // pfq-lang
    //
    auto comp = bridge(b.to);

    q.join_group(gid, group_policy::shared, class_mask::control);
    q.bind_group(gid, b.from.c_str(), any_queue);
    q.set_group_computation(gid, comp);
}



void sighandler(int)
{
    opt::stop.store(true, std::memory_order_relaxed);
}

int
main(int argc, char *argv[])
try
{
    std::vector<Bridge> bridges;

    if (argc < 2)
        usage(argv[0]);

    signal(SIGINT, sighandler);
    signal(SIGSTOP, sighandler);

    for(int i = 1; i < argc; ++i)
    {
        if (any_strcmp(argv[i], "-b", "--bridge")) {

            i+=2;
            if (i >= argc)
            {
                throw std::runtime_error("bridge: argument(s) missing");
            }

            bridges.emplace_back(true, 0, 0, argv[i-1], argv[i]);
            continue;
        }

        if (any_strcmp(argv[i], "-f", "--forward")) {
            i+=4;
            if (i >= argc)
            {
                throw std::runtime_error("forward: argument(s) missing");
            }

            bridges.emplace_back(false, atoi(argv[i-3]), atoi(argv[i-2]), argv[i-1], argv[i]);
            continue;
        }

        if (any_strcmp(argv[i], "--fast"))
        {
            opt::fast_forward = true;
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

        if (any_strcmp(argv[i], "-h", "-?", "--help"))
            usage(argv[0]);

        throw std::runtime_error(std::string(argv[i]) + " unknown option!");
    }

    std::cout << "pfq-bridge!" << std::endl;

    pfq::socket q(group_policy::undefined, 64, 1024);

    // load in-kernel pfq-lang bridges...
    //

    int gid = 0;
    for(auto & b : bridges)
    {
        if (b.kernel)
            make_lang_bridge(q, gid++, b);
        else
            make_user_bridge(gid++, b);
    }

    // wait...
    //

    for(;;) {
        std::this_thread::sleep_for(std::chrono::seconds(1));
        if (opt::stop.load(std::memory_order_relaxed))
            break;
    }

    std::cout << "closing..." << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(1));

}
catch(std::exception &e)
{
    std::cerr << e.what() << std::endl;
}
