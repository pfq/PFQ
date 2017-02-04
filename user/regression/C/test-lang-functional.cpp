#include <iostream>
#include <string>
#include <stdexcept>
#include <chrono>

#include <pfq/pfq.hpp>
#include <pfq/lang/lang.hpp>
#include <pfq/lang/default.hpp>
#include <pfq/lang/experimental.hpp>

using namespace pfq::lang;
using namespace pfq::lang::experimental;

int
main(int argc, char *argv[])
{
    if (argc < 2)
        throw std::runtime_error(std::string("usage: ").append(argv[0]).append(" dev"));

    pfq::socket q(128);

    q.bind(argv[1], pfq::any_queue);

    auto gid = q.group_id();

    // ip >-> inc 0
    // >-> conditional is_icmp
    //     (inc 1 >-> mark 1 >-> double_steer_ip >-> when' (has_mark 1) (inc 2))
    //     drop'

    auto comp = ip >> icmp >> forward ("lo") >> steer_local_link("4c:60:de:86:55:46") >> addr("192.168.0.0/16") >> unit >> inc (0) >>
                    conditional (is_icmp & has_addr("192.168.0.0/16"),
                                 (inc (1) >> mark (1) >> double_steer_ip >> when (has_mark (1), inc (2))),
                                  drop);


    std::cout << pretty (comp) << std::endl;

    q.set_group_computation(gid, comp);

    std::this_thread::sleep_for(std::chrono::seconds(1));

}

