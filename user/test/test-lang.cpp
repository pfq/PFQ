#include <iostream>

#include "pfq-lang.hpp"

using namespace pfq_lang;

template <typename C>
void show_descr(C const &c)
{
    auto s = serialize(0, c);
    int n = 0;

    for(auto &term : s.first)
    {
        std::cout << n++ << ' ' << show(term) << std::endl;
    }
}

int
main()
{
     //////// predicates:

     auto or_  = combinator("or");
     auto and_ = combinator("and");

     auto p0 = predicate("is_ip");
     auto p1 = predicate("has_value", 42);
     auto p2 = predicate(or_, p0, p1);
     auto p3 = predicate(and_, p2, p2);

     //////// computations:

     auto c0  = computation("fun");
     auto c1  = computation("fun1", 42);

     auto c20 = computation("hfun", p0);
     auto c21 = computation("hfun", p1);
     auto c22 = computation("hfun", p2);
     auto c23 = computation("hfun", p3);

     auto c30 = computation("hfun1", p0, c0);
     auto c31 = computation("hfun1", p1, c0);
     auto c32 = computation("hfun1", p0, c1);
     auto c33 = computation("hfun1", p1, c1);
     auto c34 = computation("hfun1", p0, c20);
     auto c35 = computation("hfun1", p1, c21);

     auto c40 = computation("hfun2", p0, c0, c1);
     auto c41 = computation("hfun2", p1, c0, c1);
     auto c42 = computation("hfun2", p0, c20, c21);
     auto c43 = computation("hfun2", p1, c20, c21);

     auto c50 = c0 >> c1;
     auto c51 = c1 >> c20 >> c21;
     auto c52 = c30 >> c40;

     std::cout << show(p0) << std::endl;
     std::cout << show(p1) << std::endl;
     std::cout << show(p2) << std::endl;
     std::cout << show(p3) << std::endl;

     std::cout << show(c0) << std::endl;
     std::cout << show(c1) << std::endl;
     std::cout << show(c20) << std::endl;
     std::cout << show(c21) << std::endl;
     std::cout << show(c22) << std::endl;
     std::cout << show(c23) << std::endl;

     std::cout << show(c30) << std::endl;
     std::cout << show(c31) << std::endl;
     std::cout << show(c32) << std::endl;
     std::cout << show(c33) << std::endl;
     std::cout << show(c34) << std::endl;
     std::cout << show(c35) << std::endl;

     std::cout << show(c40) << std::endl;
     std::cout << show(c41) << std::endl;
     std::cout << show(c42) << std::endl;
     std::cout << show(c43) << std::endl;

     std::cout << show(c50) << std::endl;
     std::cout << show(c51) << std::endl;
     std::cout << show(c52) << std::endl;

     std::cout << "---" << std::endl;
     show_descr (p0);
     std::cout << "---" << std::endl;
     show_descr (p1);
     std::cout << "---" << std::endl;
     show_descr (p2);
     std::cout << "---" << std::endl;
     show_descr (p3);

     std::cout << "---" << std::endl;
     show_descr (c0);
     std::cout << "---" << std::endl;
     show_descr (c1);
     std::cout << "---" << std::endl;
     show_descr (c20);

     std::cout << "---" << std::endl;
     show_descr (c30);

     std::cout << "---" << std::endl;
     show_descr (c41);

     std::cout << "---" << std::endl;
     show_descr (c52);

     return 0;
}

