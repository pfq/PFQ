#include <iostream>

#include <pfq/lang/default.hpp>
#include <pfq/lang/experimental.hpp>

using namespace pfq::lang;
using namespace pfq::lang::experimental;

int
main()
{
    // par functions:

    std::cout << pretty ( par3 (ip, ip, ip) ) << std::endl;
    std::cout << pretty ( par4 (ip, ip, ip, ip) ) << std::endl;
    std::cout << pretty ( par5 (ip, ip, ip, ip, ip) ) << std::endl;
    std::cout << pretty ( par6 (ip, ip, ip, ip, ip, ip) ) << std::endl;
    std::cout << pretty ( par7 (ip, ip, ip, ip, ip, ip, ip) ) << std::endl;
    std::cout << pretty ( par8 (ip, ip, ip, ip, ip, ip, ip, ip) ) << std::endl;

    return 0;
}

