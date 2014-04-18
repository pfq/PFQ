#include <iostream>

#include "pfq-lang.hpp"

using namespace pfq_lang;

template <typename C>
void show_cont(C const &c)
{
    std::cout << "---" << std::endl;
    for(auto &elem : c.first)
    {
        std::cout << show(elem) << std::endl;
    }
}

int
main()
{
     auto or_  = combinator("or");
     auto and_ = combinator("and");

     auto p0 = predicate("is_ip");
     auto p1 = predicate("has_value", 42);
     auto p2 = predicate(or_, p0, p1);
     auto p3 = predicate(and_, p2, p2);

     std::cout << show(p0) << std::endl;
     std::cout << show(p1) << std::endl;
     std::cout << show(p2) << std::endl;
     std::cout << show(p3) << std::endl;

     auto s0 = serialize(0, p0);
     auto s1 = serialize(0, p1);
     auto s2 = serialize(0, p2);
     auto s3 = serialize(0, p3);

     show_cont (s0);
     show_cont (s1);
     show_cont (s2);
     show_cont (s3);

     return 0;
}

