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

#include <pfq.hpp>
#include <pfq-lang/lang.hpp>
#include <pfq-lang/default.hpp>
#include <pfq-lang/experimental.hpp>

#include <affinity.hpp>
#include <binding.hpp>
#include <vt100.hpp>

using namespace pfq;
using namespace pfq::lang;
using namespace pfq::lang::experimental;


void usage(std::string name)
{
    throw std::runtime_error
    (
        "usage: " + std::move(name) + " [OPTIONS]\n\n"
        " -h --help                     Display this help\n"
        " -b --bridge DEV1 DEV2         Unidirectional bridge: DEV1 -> DEV2\n"
    );
}


void make_bridge(pfq::socket &q, int gid, std::pair<std::string, std::string> const &b)
{
        std::cout << "group " << gid << " :: bridge [" << b.first << " -> " << b.second << "]" << std::endl;

        auto comp = bridge(b.second);

        q.join_group(gid, group_policy::shared, class_mask::control);
        q.bind_group(gid, b.first.c_str(), any_queue);
        q.set_group_computation(gid, comp);
}


int
main(int argc, char *argv[])
try
{
    std::vector<std::pair<std::string, std::string>> bridges;

    if (argc < 2)
        usage(argv[0]);

    for(int i = 1; i < argc; ++i)
    {
        if ( strcmp(argv[i], "-b") == 0 ||
             strcmp(argv[i], "--bridge") == 0) {
            i+=2;
            if (i >= argc)
            {
                throw std::runtime_error("descriptor missing");
            }

            bridges.push_back( std::make_pair(argv[i-1], argv[i]) );
            continue;
        }

        if ( strcmp(argv[i], "-h") == 0 ||
             strcmp(argv[i], "-?") == 0 ||
             strcmp(argv[i], "--help") == 0
             )
            usage(argv[0]);

        throw std::runtime_error(std::string(argv[i]) + " unknown option!");
    }

    std::cout << "pfq-bridge!" << std::endl;

    pfq::socket q(group_policy::undefined, 64, 1024);

    int gid = 0;
    for(auto & b : bridges)
    {
        make_bridge(q, gid++, b);
    }

    for(;;)
        std::this_thread::sleep_for(std::chrono::seconds(1));

    return 0;
}
catch(std::exception &e)
{
    std::cerr << e.what() << std::endl;
}
