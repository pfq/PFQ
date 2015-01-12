#include <iostream>

#include <pfq/pfq.hpp>
#include <pfq/lang/default.hpp>
#include <pfq/lang/experimental.hpp>

using namespace pfq::lang;
using namespace pfq::lang::experimental;

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

    // par functions:

    check_computation( q, par3 (ip, ip, ip) );
    check_computation( q, par4 (ip, ip, ip, ip) );
    check_computation( q, par5 (ip, ip, ip, ip, ip) );
    check_computation( q, par6 (ip, ip, ip, ip, ip, ip) );
    check_computation( q, par7 (ip, ip, ip, ip, ip, ip, ip) );
    check_computation( q, par8 (ip, ip, ip, ip, ip, ip, ip, ip) );

    return 0;
}

