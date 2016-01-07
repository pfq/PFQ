#include <iostream>

#include <pfq/lang/lang.hpp>


using namespace pfq::lang;


template <typename C>
void show_comp(C const &c)
{
    std::cout << "*** show: "   << show (c) << std::endl;
    std::cout << "*** pretty: " << pretty (c) << std::endl;
    std::cout << "*** serialized:\n";

    auto s = serialize(c, 0);
    int n = 0;

    for(auto &term : s.first)
    {
        std::cout << n++ << ' ' << show(term) << std::endl;
    }

    std::cout << std::endl;
}


int
main()
{
    auto fun0  = function("fun");
    auto fun1  = [](int n) { return function("fun1", n); };
    auto fun2  = [](std::string s) { return function("fun", std::move(s)); };

    auto prop0 = property("prop0");
    auto prop1 = [](int n ) { return property("prop1", n); };

    auto pred0 = predicate("is_ip");
    auto pred1 = [] (int n) { return predicate("has_port", n); };
    auto pred2 = [] (auto pro) { return predicate("pred2", pro); };
    auto pred3 = [] (auto pro, int n) { return predicate("pred3", pro, n); };

    auto not_  = [] (auto pred)        { return predicate("not", pred); };
    auto or_   = [] (auto p1, auto p2) { return predicate("or", p1, p2); };
    auto and_  = [] (auto p1, auto p2) { return predicate("and", p1, p2); };

    auto hfun  = [] (auto p) { return function("hfun", p); };

    auto cond  = [] (auto p, auto c1, auto c2) { return function("cond", p, c1, c2); };

    auto integers = function("int", std::vector<int>{1, 2, 3});
    auto strings  = function("str", std::vector<std::string>{"one", "two", "tree"});

    show_comp (fun0);
    show_comp (fun1(42));
    show_comp (fun2("hello"));

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
    show_comp (cond(pred0, fun0, fun1(3)));

    show_comp (fun0 >> fun1(1) >> fun2 ("test"));

    show_comp (integers);
    show_comp (strings);


    return 0;
}

