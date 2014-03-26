#include <iostream>

#include "pfq-lang.hpp"

using namespace pfq_lang;

int
main()
{
     int value = 42;

     auto comp = ip() >> tcp() >> dummy(value);

     std::cout << "computation: " << show(comp) << std::endl;

     auto ptr = eval(comp());

     for(unsigned int i = 0; i < ptr->size; i++)
     {
        std::cout << "fun: " << ptr->fun[i].symbol;
        if (ptr->fun[i].context.size)
        {
            std::cout << " context@" << (void *)ptr->fun[i].context.addr << " size:" << ptr->fun[i].context.size;
        }

        std::cout << std::endl;
     }

     return 0;
}

