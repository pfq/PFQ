#include <iostream>

#include "pfq-lang/default.hpp"

using namespace pfq_lang;

int
main()
{
    // predicates and combinators:

    std::cout << show (is_tcp) << std::endl;
    std::cout << show (has_mark(42)) << std::endl;
    std::cout << show ( is_ip  & is_tcp ) << std::endl;
    std::cout << show ( is_udp | is_tcp ) << std::endl;
    std::cout << show ( is_ip6 ^ has_mark(11) ) << std::endl;
    std::cout << show ( is_ip & ( is_tcp | is_udp) ) << std::endl;

    // computations:

    std::cout << show ( ip >> udp >> steer_rtp >> counter(2) ) << std::endl;

    std::cout << show ( hdummy (is_ip) ) << std::endl;
    std::cout << show ( hdummy (is_ip) ) << std::endl;

    std::cout << show ( when   (has_vid(1), ip >> steer_ip) ) << std::endl;
    std::cout << show ( unless (is_ip, ip >> steer_ip) ) << std::endl;
    std::cout << show ( conditional (is_ip, steer_ip, drop  ) ) << std::endl;

    return 0;
}

