#include <iostream>

#include "pfq-lang/default.hpp"

using namespace pfq_lang;

int
main()
{
    // predicates and combinators:

    std::cout << pretty (is_tcp) << std::endl;
    std::cout << pretty (has_mark(42)) << std::endl;
    std::cout << pretty ( is_ip  & is_tcp ) << std::endl;
    std::cout << pretty ( is_udp | is_tcp ) << std::endl;
    std::cout << pretty ( is_ip6 ^ has_mark(11) ) << std::endl;
    std::cout << pretty ( is_ip & ( is_tcp | is_udp) ) << std::endl;

    // computations:

    std::cout << pretty ( ip >> udp >> steer_rtp >> inc(2) ) << std::endl;

    std::cout << pretty ( hdummy (is_ip) ) << std::endl;
    std::cout << pretty ( hdummy (is_ip) ) << std::endl;

    std::cout << pretty ( when   (has_vid(1), ip >> steer_ip) ) << std::endl;
    std::cout << pretty ( unless (is_ip, ip >> steer_ip) ) << std::endl;
    std::cout << pretty ( conditional (is_ip, steer_ip, drop  ) ) << std::endl;

    return 0;
}

