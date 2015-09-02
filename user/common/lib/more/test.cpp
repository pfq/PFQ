#include "binding.hpp"

#include <iostream>

int
main(int argc, char *argv[])
{
    if (argc == 2)
        std::cout << argv[1] << std::endl;

    std::cout << "    " << more::netdev_format << std::endl;
    std::cout << "    " << more::thread_binding_format << std::endl;

    auto a0 = more::read_thread_binding("0.1");
    auto a1 = more::read_thread_binding("0.1.eth0");
    auto a2 = more::read_thread_binding("0.1.eth0:1");
    auto a3 = more::read_thread_binding("0.1.eth0:1,2.eth1:3,4.eth2:-1.eth3");
    auto a4 = more::read_thread_binding("0.1.eth0.eth1");

    std::cout << show(a0) << std::endl;
    std::cout << show(a1) << std::endl;
    std::cout << show(a2) << std::endl;
    std::cout << show(a3) << std::endl;
    std::cout << show(a4) << std::endl;

     return 0;
}

