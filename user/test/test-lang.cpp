#include <iostream>

#include "pfq-lang/lang.hpp"
#include "pfq-lang/default.hpp"

using namespace pfq_lang;

template <typename C>
void show_comp(C const &c)
{
    auto s = serialize(c, 0);
    int n = 0;

    std::cout << "*** " << pretty (c) << ":\n" << std::endl;

    for(auto &term : s.first)
    {
        std::cout << n++ << ' ' << show(term) << std::endl;
    }

    std::cout << std::endl;
}


int
main()
{
    //////// pfq_functional_descr:
    {
        auto fun = pfq_functional_descr { "test", { {(void *)0xdeadbeef, 0}, {0,0}, {0,0}, {0,0}}, 1, 2 };
        std::cout << show(fun) << std::endl;
    }

    //////// Argument:
    {
        std::string test("test");
        std::cout << show(Argument::Null()) << std::endl;
        std::cout << show(Argument::String(test)) << std::endl;
        std::cout << show(Argument::Fun(42)) << std::endl;
        std::cout << show(Argument::Data(11)) << std::endl;
    }

    //////// FunctionDescr:
    {
        std::string test("test");
        FunctionDescr descr { "fun" , {{ Argument::String(test), Argument::Data(42) }}, 1, 2 };
        std::cout << show(descr) << std::endl;
    }

    //////// predicates:

    auto fun0  = mfunction("fun");
    auto fun1  = [](int n) { return mfunction1("fun1", n); };
    auto fun2  = [](std::string s) { return mfunction2("fun", std::move(s)); };

    auto prop0 = property("prop0");
    auto prop1 = [](int n ) { return property1("prop1", n); };

    auto pred0 = predicate("is_ip");
    auto pred1 = [] (int n) { return predicate1("has_port", n); };
    auto pred2 = [] (auto pro) { return predicate2("pred2", pro); };
    auto pred3 = [] (auto pro, int n) { return predicate3("pred3", pro, n); };

    auto not_  = [] (auto pred) { return combinator1("not", pred); };
    auto or_   = [] (auto p1, auto p2) { return combinator2("or", p1, p2); };
    auto and_  = [] (auto p1, auto p2) { return combinator2("and", p1, p2); };

    auto hfun  = [] (auto p) { return hfunction("hfun", p); };

    auto when  = [] (auto p, auto c) { return hfunction1("when", p, c); };
    auto cond  = [] (auto p, auto c1, auto c2) { return hfunction2("cond", p, c1, c2); };

    auto comp  = fun0 >> fun1 (10) >> fun0 >> hfun (pred1(42)) >> when (pred0, fun0) >> cond ( pred1(11), fun0, fun1(12));

    std::cout << "---" << std::endl;


    std::cout << pretty (comp) << std::endl;

    show_comp (fun0);
    show_comp (fun1(42));
    show_comp (fun2("hello"));
    show_comp (fun0 >> fun1(1) >> fun2 ("test"));

    show_comp (prop0);
    show_comp (prop1 (1));

    show_comp (pred0);
    show_comp (pred1(1));
    show_comp (pred2(prop0));
    show_comp (pred3(prop0, 10));
    show_comp (not_(pred0));

    show_comp (or_(pred0, pred1(1)));
    show_comp (and_(pred0, or_(pred1(1), pred1(2)) ));

    show_comp (hfun(pred0));

    show_comp (when(pred0, fun0) );
    show_comp (cond(pred0, fun0, fun1(3)));

    return 0;
}

