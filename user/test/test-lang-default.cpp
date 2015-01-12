#include <iostream>

#include <pfq/pfq.hpp>
#include <pfq/lang/default.hpp>

using namespace pfq::lang;

template <typename Comp>
void
check_computation(pfq::socket &q, Comp comp)
{
    auto gid = q.group_id();

    std::cout << pretty(comp) << std::endl;

    q.set_group_computation(gid, comp);
}


int
main()
{
    pfq::socket q(128);

    // predicates and combinators:

    check_computation(q, filter(is_tcp));
    check_computation(q, filter(has_mark(42)));
    check_computation(q, filter(is_ip  & is_tcp ));
    check_computation(q, filter(is_udp | is_tcp ));
    check_computation(q, filter(is_ip6 ^ has_mark(11) ));
    check_computation(q, filter(is_ip & ( is_tcp | is_udp) ));

    // computations:

    check_computation(q, ip >> udp >> inc(2) );
    check_computation(q, when   (has_vid(1), ip >> steer_ip) );
    check_computation(q, unless (is_ip, ip >> steer_ip) );
    check_computation(q, conditional (is_ip, steer_ip, drop  ) );

    return 0;
}

