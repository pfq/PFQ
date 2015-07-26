/******************************************************************************
 *
 * The MIT License (MIT)
 *
 * Copyright (c) 2011-15 Nicola Bonelli <nicola@pfq.io>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 ******************************************************************************/

/////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                             //
//  YATS: Yet Another Test Suite. Get the more recent release at https://github.com/awgn/yats  //
//                                                                                             //
/////////////////////////////////////////////////////////////////////////////////////////////////


#ifndef _YATS_HPP_
#define _YATS_HPP_

#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <functional>
#include <memory>
#include <string>
#include <cstring>
#include <vector>
#include <stdexcept>
#include <csignal>
#include <chrono>
#include <set>
#include <map>

#ifdef __GNUC__
#include <cxxabi.h>
#endif


////////////////////////////////////////////// runtime assert:

#define Assert(...)                 yats::assert        (__FILE__, __LINE__, 0, __VA_ARGS__)
#define AssertNoThrow(...)          yats::assert_throw  (__FILE__, __LINE__, 0, [&](){ __VA_ARGS__; }, nothing())
#define AssertThrow(...)            yats::assert_throw  (__FILE__, __LINE__, 0, [&](){ __VA_ARGS__; }, anything())
#define AssertThrowAs(e,...)        yats::assert_throw  (__FILE__, __LINE__, 0, [&](){ __VA_ARGS__; }, e)

#define AssertId(n, ...)            yats::assert        (__FILE__, __LINE__, n, __VA_ARGS__)
#define AssertNoThrowId(n, ...)     yats::assert_throw  (__FILE__, __LINE__, n, [&](){ __VA_ARGS__; }, nothing())
#define AssertThrowId(n, ...)       yats::assert_throw  (__FILE__, __LINE__, n, [&](){ __VA_ARGS__; }, anything())
#define AssertThrowAsId(n, e,...)   yats::assert_throw  (__FILE__, __LINE__, n, [&](){ __VA_ARGS__; }, e)

////////////////////////////////////////////// static assert:

#define StaticError(expr,msg)       YATS_XPASTE(YATS_STATIC_ERROR_, __COUNTER__) (expr,msg)

#define YATS_PASTE(a,b)             a ## b
#define YATS_XPASTE(a,b)            YATS_PASTE(a,b)

#define YATS_STATIC_ERROR_CLASS(expr, msg) \
    struct static_error {\
        static_error() \
        {  \
            expr; \
            std::cerr << "YATS: Static error failure: test " # expr ": " msg " is falsifiable." << std::endl; \
            _Exit(EXIT_FAILURE);\
        } \
    } maybe_error_ = static_error();


#if defined(YATS_STATIC_ERROR) && YATS_STATIC_ERROR == 0
#define YATS_STATIC_ERROR_0(expr,msg) YATS_STATIC_ERROR_CLASS(expr,msg);
#else
#define YATS_STATIC_ERROR_0(expr,msg)
#endif
#if defined(YATS_STATIC_ERROR) && YATS_STATIC_ERROR == 1
#define YATS_STATIC_ERROR_1(expr,msg) YATS_STATIC_ERROR_CLASS(expr,msg);
#else
#define YATS_STATIC_ERROR_1(expr,msg)
#endif
#if defined(YATS_STATIC_ERROR) && YATS_STATIC_ERROR == 2
#define YATS_STATIC_ERROR_2(expr,msg) YATS_STATIC_ERROR_CLASS(expr,msg);
#else
#define YATS_STATIC_ERROR_2(expr,msg)
#endif
#if defined(YATS_STATIC_ERROR) && YATS_STATIC_ERROR == 3
#define YATS_STATIC_ERROR_3(expr,msg) YATS_STATIC_ERROR_CLASS(expr,msg);
#else
#define YATS_STATIC_ERROR_3(expr,msg)
#endif
#if defined(YATS_STATIC_ERROR) && YATS_STATIC_ERROR == 4
#define YATS_STATIC_ERROR_4(expr,msg) YATS_STATIC_ERROR_CLASS(expr,msg);
#else
#define YATS_STATIC_ERROR_4(expr,msg)
#endif
#if defined(YATS_STATIC_ERROR) && YATS_STATIC_ERROR == 5
#define YATS_STATIC_ERROR_5(expr,msg) YATS_STATIC_ERROR_CLASS(expr,msg);
#else
#define YATS_STATIC_ERROR_5(expr,msg)
#endif
#if defined(YATS_STATIC_ERROR) && YATS_STATIC_ERROR == 6
#define YATS_STATIC_ERROR_6(expr,msg) YATS_STATIC_ERROR_CLASS(expr,msg);
#else
#define YATS_STATIC_ERROR_6(expr,msg)
#endif
#if defined(YATS_STATIC_ERROR) && YATS_STATIC_ERROR == 7
#define YATS_STATIC_ERROR_7(expr,msg) YATS_STATIC_ERROR_CLASS(expr,msg);
#else
#define YATS_STATIC_ERROR_7(expr,msg)
#endif
#if defined(YATS_STATIC_ERROR) && YATS_STATIC_ERROR == 8
#define YATS_STATIC_ERROR_8(expr,msg) YATS_STATIC_ERROR_CLASS(expr,msg);
#else
#define YATS_STATIC_ERROR_8(expr,msg)
#endif
#if defined(YATS_STATIC_ERROR) && YATS_STATIC_ERROR == 9
#define YATS_STATIC_ERROR_9(expr,msg) YATS_STATIC_ERROR_CLASS(expr,msg);
#else
#define YATS_STATIC_ERROR_9(expr,msg)
#endif


namespace yats
{
    ////////////////////////////////////////////// global instance:

    struct global
    {
        std::string program_name;

        size_t assert_ok;
        size_t assert_total;

        std::vector<struct Group *> groups;
        std::set<std::string> group_names;

        std::set<std::tuple<std::string, int, int>> yats_assert;

        static global&
        instance()
        {
            static global one;
            return one;
        }

        global(global const&  other) = delete;
        global& operator=(global const&  other) = delete;

    private:
        global() = default;
    };

    ////////////////////////////////////////////// unix signal:

    namespace detail
    {
        static const std::map<int, std::string> &
        unix_signal_map()
        {
            static std::map<int, std::string> m = []() {
                std::map<std::string, std::vector<int>> reverse
                = {{"SIGHUP"  ,  {     1    }},
                   {"SIGINT"  ,  {     2    }},
                   {"SIGQUIT" ,  {     3    }},
                   {"SIGILL"  ,  {     4    }},
                   {"SIGABRT" ,  {     6    }},
                   {"SIGFPE"  ,  {     8    }},
                   {"SIGKILL" ,  {     9    }},
                   {"SIGSEGV" ,  {    11    }},
                   {"SIGPIPE" ,  {    13    }},
                   {"SIGALRM" ,  {    14    }},
                   {"SIGTERM" ,  {    15    }},
                   {"SIGUSR1" ,  { 30,10,16 }},
                   {"SIGUSR2" ,  { 31,12,17 }},
                   {"SIGCHLD" ,  { 20,17,18 }},
                   {"SIGCONT" ,  { 19,18,25 }},
                   {"SIGSTOP" ,  { 17,19,23 }},
                   {"SIGTSTP" ,  { 18,20,24 }},
                   {"SIGTTIN" ,  { 21,21,26 }},
                   {"SIGTTOU" ,  { 22,22,27 }},
                   {"SIGBUS"  ,  { 10,7,10  }},
                   {"SIGPROF" ,  { 27,27,29 }},
                   {"SIGSYS"  ,  { 12,31,12 }},
                   {"SIGTRAP" ,  {    5     }},
                   {"SIGURG"  ,  { 16,23,21 }},
                   {"SIGVTALRM", { 26,26,28 }},
                   {"SIGXCPU" ,  { 24,24,30 }},
                   {"SIGXFSZ" ,  { 25,25,31 }}};

                std::map<int, std::string> sigmap;

                for (auto const & sig : reverse)
                    for(auto const &signum : sig.second)
                        sigmap.insert(std::make_pair(signum, sig.first));

                return sigmap;
            }();

            return m;
        }

        static std::string
        unix_signal(int n)
        {
            auto it = detail::unix_signal_map().find(n);
            if (it != std::end(detail::unix_signal_map()))
                return it->second;
            return std::string("SIGNUM ") + std::to_string(n);
        }
    }

    static void sig_handler(int n)
    {
        std::ofstream ferr("/tmp/" + global::instance().program_name, std::fstream::app);
        ferr << detail::unix_signal(n) << std::endl;
        _Exit (-n);
    }

    ////////////////////////////////////////////// C++ yats exception:

    struct yats_error : public std::runtime_error
    {
        explicit yats_error(std::string file, int line, const std::string &msg)
        : std::runtime_error(msg)
        , file_(std::move(file))
        , line_(line)
        { }

        yats_error(const yats_error &) = default;

        virtual ~yats_error() noexcept { }

        std::string file_;
        int line_;
    };

    ////////////////////////////////////////////// nothing and anything:

    struct nothing
    {
        static constexpr const char * what() { return "nothing"; }
    };

    inline std::string
    type_name(nothing)
    {
        return "no";
    }

    struct anything
    {
        static constexpr const char * what() { return "anything"; }
    };

    inline std::string
    type_name(anything)
    {
        return "any";
    }

    ////////////////////////////////////////////// C++ demangling tool:

    inline std::string
    cxa_demangle(const char *name)
    {
#ifdef __GNUC__
        int status;
        std::unique_ptr<char, void (*)(void *)> ret(abi::__cxa_demangle(name,0,0, &status), ::free);
        if (status < 0)
            throw yats_error("", 0, "__cxa_demangle");
        return std::string(ret.get());
#else
        throw std::runtime_error("YATS: demangle not supported");
#endif
    }

    template <typename Tp>
    inline std::string
    type_name(const Tp &t)
    {
        return cxa_demangle(typeid(t).name());
    }

    ////////////////////////////////////////////// duration:

    template <typename Dur>
    std::string duration_to_string(Dur d)
    {
        if (d < std::chrono::milliseconds(10))
            return std::to_string(std::chrono::duration_cast<std::chrono::microseconds>(d).count()) + " us";
        if (d < std::chrono::seconds(10))
            return std::to_string(static_cast<double>(std::chrono::duration_cast<std::chrono::microseconds>(d).count())/1000.0) + " ms";
        else
            return std::to_string(static_cast<double>(std::chrono::duration_cast<std::chrono::microseconds>(d).count())/1000000.0) + " s";
    }

    ////////////////////////////////////////////// utilities:

    template <typename CharT, typename Traits, typename T>
    void print(std::basic_ostream<CharT,Traits> &out, T &&arg)
    {
        out << std::forward<T>(arg);
    }
    template <typename CharT, typename Traits, typename T, typename ...Ti>
    void print(std::basic_ostream<CharT, Traits> &out, T &&arg, Ti&&... args)
    {
        out << arg; print(out, std::forward<Ti>(args)...);
    }

    template <typename ...Ts>
    std::string
    make_string(Ts&&... args)
    {
        std::ostringstream out;
        out << std::boolalpha;
        print(out, std::forward<Ts>(args)...);
        return out.str();
    }

    template <typename ...Ts>
    std::string
    make_error(const char *file, int line, Ts&&... args)
    {
        return make_string(file, ':', line, ":\n", std::forward<Ts>(args)...);
    }

    ////////////////////////////////////////////// type traits:

    struct __sfinae_types
    {
        using __one = char;
        using __two = struct { char __arr[2]; };
    };

    template <typename T>
    class has_insertion_operator : public __sfinae_types
    {
        template <typename C> static __one test(typename std::remove_reference<decltype(std::cout << std::declval<C>())>::type *);
        template <typename C> static __two test(...);
    public:
        enum { value = sizeof(test<T>(0)) == sizeof(__one) };
    };

    template <typename T>
    class is_yats_expression : public __sfinae_types
    {
        template <typename C> static __one test(typename std::remove_reference<typename C::yats_expression>::type *);
        template <typename C> static __two test(...);
    public:
        enum { value = sizeof(test<T>(0)) == sizeof(__one) };
    };

    ////////////////////////////////////////////// pretty printer values:

    static inline std::string pretty(bool v)
    {
        std::ostringstream o;
        o << std::boolalpha << v;
        return o.str();
    }
    template <typename T>
    typename std::enable_if<std::is_integral<T>::value, std::string>::type
    pretty(const T &v)
    {
        std::ostringstream o;
        o << std::boolalpha << v;
        if (v > 15)
            o << '^' << std::hex << "0x" << v << std::dec;
        return o.str();
    }
    template <typename T>
    typename std::enable_if<!std::is_integral<T>::value &&
                            has_insertion_operator<T>::value, std::string>::type
    pretty(const T &v)
    {
        std::ostringstream o;
        o << v;
        return o.str();
    }
    template <typename T>
    typename std::enable_if<!std::is_integral<T>::value &&
                            !has_insertion_operator<T>::value, std::string>::type
    pretty(const T &)
    {
        return "()";
    }

    ////////////////////////////////////////////// groups:

    template <typename Fun>
    using task = std::pair<std::string, std::function<Fun>>;

    struct Group
    {
        Group(std::string name)
        : name_(std::move(name))
        {
            if (!global::instance().group_names.insert(name_).second)
                throw std::runtime_error("Group name '" + name_ + "' already in use!");

            global::instance().groups.push_back(this);
        }

        Group(Group const &) = delete;
        Group& operator=(Group const &) = delete;

        Group(Group &&other) noexcept
        : name_    (std::move(other.name_))
        , setup_   (std::move(other.setup_))
        , teardown_(std::move(other.teardown_))
        , prolog_  (std::move(other.prolog_))
        , test_    (std::move(other.test_))
        , epilog_  (std::move(other.epilog_))
        {
            *std::find(std::begin(global::instance().groups), std::end(global::instance().groups), &other) = this;
        }

        Group& operator=(Group && other)
        {
            name_     = std::move(other.name_);
            setup_    = std::move(other.setup_);
            teardown_ = std::move(other.teardown_);
            prolog_   = std::move(other.prolog_);
            test_     = std::move(other.test_);
            epilog_   = std::move(other.epilog_);

            *std::find(std::begin(global::instance().groups), std::end(global::instance().groups), &other) = this;
            return *this;
        }

        template <typename Fun>
        Group &&
        Setup(Fun f) &&
        {
            setup_.emplace_back("setup", std::move(f));
            return std::move(*this);
        }
        template <typename Fun>
        Group &
        Setup(Fun f) &
        {
            setup_.emplace_back("setup", std::move(f));
            return *this;
        }

        template <typename Fun>
        Group &&
        Teardown(Fun f) &&
        {
            teardown_.emplace_back("teardown", std::move(f));
            return std::move(*this);
        }
        template <typename Fun>
        Group &
        Teardown(Fun f) &
        {
            teardown_.emplace_back("teardown", std::move(f));
            return *this;
        }

        template <typename Fun>
        Group &&
        Prolog(Fun f) &&
        {
            prolog_.emplace_back("prolog", std::move(f));
            return std::move(*this);
        }
        template <typename Fun>
        Group &
        Prolog(Fun f) &
        {
            prolog_.emplace_back("prolog", std::move(f));
            return *this;
        }

        template <typename Fun>
        Group &&
        Epilog(Fun f) &&
        {
            epilog_.emplace_back("epilog", std::move(f));
            return std::move(*this);
        }
        template <typename Fun>
        Group &
        Epilog(Fun f) &
        {
            epilog_.emplace_back("epilog", std::move(f));
            return *this;
        }

        template <typename Fun>
        Group &
        Single(std::string name, Fun f) &
        {
            check_unique_test_name(name);
            test_.emplace_back(std::move(name), [=] (int) { f(); });
            return *this;
        }
        template <typename Fun>
        Group &&
        Single(std::string name, Fun f) &&
        {
            check_unique_test_name(name);
            test_.emplace_back(std::move(name), [=] (int) { f(); });
            return std::move(*this);
        }

        template <typename Fun>
        Group &
        Repeat(std::string name, Fun f) &
        {
            check_unique_test_name(name);
            test_.emplace_back(std::move(name), [=](int run) {
                                    for(int i = 0; i < run; i++)
                                        f();
                                  });
            return *this;
        }
        template <typename Fun>
        Group &&
        Repeat(std::string name, Fun f) &&
        {
            check_unique_test_name(name);
            test_.emplace_back(std::move(name), [=](int run) {
                                    for(int i = 0; i < run; i++)
                                        f();
                                  });
            return std::move(*this);
        }

        void check_unique_test_name(std::string const &name)
        {
            if (!test_names_.insert(name).second)
                throw std::runtime_error("YATS: in Group '" + name_ + "', name '" + name + "' already in use!");
        }

        std::string name_;

        std::vector<task<void()>> setup_;
        std::vector<task<void()>> teardown_;
        std::vector<task<void()>> prolog_;
        std::vector<task<void(int)>> test_;
        std::vector<task<void()>> epilog_;

        std::set<std::string> test_names_;
    };

    ////////////////////////////////////////////// usage:

    static void usage(const char *name)
    {
        std::cout << "Yats usage: " << name << " [options] [test...]" << std::endl;
        std::cout << "Options:\n";
        std::cout << "  -e, --exit-immediately  On error exit.\n";
        std::cout << "  -g, --group group       Run tests from the given group.\n";
        std::cout << "  -v, --verbose           Verbose mode.\n";
        std::cout << "  -s, --signal            Capture unix signals.\n";
        std::cout << "  -r, --run int           Number of run per repeatable test (1000 default).\n";
        std::cout << "  -l, --list              Print the list of tests\n";
        std::cout << "  -h, --help              Print this help.\n";

        _Exit(EXIT_SUCCESS);
    }

    ////////////////////////////////////////////// YATS!

    static int run(int argc = 0, char *argv[] = nullptr)
    try
    {
        bool exit_immediatly = false,
             verbose         = false,
             capture_signal  = false;
        int  repeat_run      = 1000;

        std::set<std::string> run_ctx, run_test;

        global::instance().program_name = argv[0];

        for(auto arg = argv + 1; argv && (arg != argv + argc); ++arg)
        {
            if (strcmp(*arg, "-h") == 0 ||
                strcmp(*arg, "-?") == 0 ||
                strcmp(*arg, "--help") == 0) {
                usage(argv[0]);
            }

            if (strcmp(*arg, "-e") == 0 ||
                strcmp(*arg, "--exit-immediately") == 0) {
                exit_immediatly = true;
                continue;
            }

            if (strcmp(*arg, "-s") == 0 ||
                strcmp(*arg, "--signal") == 0) {
                capture_signal = true;
                continue;
            }

            if (strcmp(*arg, "-v") == 0 ||
                strcmp(*arg, "--verbose") == 0) {
                verbose = true;
                continue;
            }

            if (strcmp(*arg, "-g") == 0 ||
                strcmp(*arg, "--group") == 0) {
                if (++arg == (argv+argc))
                    throw std::runtime_error("YATS: number of repetition missing");
                run_ctx.insert(*arg);
                continue;
            }

            if (strcmp(*arg, "-r") == 0 ||
                strcmp(*arg, "--run") == 0) {
                if (++arg == (argv+argc))
                    throw std::runtime_error("YATS: group missing");
                repeat_run = atoi(*arg);
                continue;
            }

            if (strcmp(*arg, "-l") == 0 ||
                strcmp(*arg, "--list") == 0) {

                for(auto & ctx : global::instance().groups)
                {
                    std::cout << "Group " << ctx->name_ << ": ";
                    int n = 1;
                    for(auto & t : ctx->test_)
                    {
                        std::cout << t.first << ' ';
                        if ((n++ & 7) == 0)
                            std::cout << std::endl << "    ";
                    }
                    std::cout << std::endl;
                }
                _Exit(0);
            }

            run_test.insert(*arg);
        }

        std::cout << "YATS: verbose " << std::boolalpha << verbose << ", UNIX signals " << capture_signal << std::endl;

        if (capture_signal) {
            for(int n = 0; n < 64; n++)
                signal(n,  sig_handler);
        }

        // compute total group and total task to run...

        size_t tot_ctx = 0, tot_task = 0;

        for(auto & ctx : global::instance().groups)
        {
            if (!run_ctx.empty() && run_ctx.find(ctx->name_) == run_ctx.end())
                continue;

            tot_ctx++;
            tot_task += [&]() -> size_t
            {
                if (run_test.empty())
                    return ctx->test_.size();

                return static_cast<size_t>(
                        std::count_if(std::begin(ctx->test_),
                                     std::end(ctx->test_),
                                     [&] (task<void(int)> const &t)
                                     {
                                         return std::find(std::begin(run_test), std::end(run_test), t.first) != std::end(run_test);
                                     }));
             }();
        }

        unsigned int run = 0, ok = 0;

        std::ofstream ferr("/tmp/" + global::instance().program_name);

        std::cout << "Loading " << tot_task << " tests in " << tot_ctx << " group(s)." << std::endl;

        // iterate over groups:
        //

        for(auto & ctx : global::instance().groups)
        {
            if (!run_ctx.empty() && run_ctx.find(ctx->name_) == run_ctx.end())
                continue;

            if (verbose)
                std::cout << "Group " << ctx->name_ << ":\n";

            // run per-group setup tasks:

            for(auto & t : ctx->setup_)
                t.second();

            // run tasks...
            //

            for(auto& t : ctx->test_)
            {
                if (!run_test.empty() && run_test.find(t.first) == run_test.end())
                    continue;

                if (verbose)
                    std::cout << "+ running '" << t.first << "'... " << std::flush;

                run++;

                auto start = std::chrono::system_clock::now();

                // run prolog for this test...

                for(auto & p : ctx->prolog_)
                    p.second();

                bool retry = true,
                     err   = false;
                do
                {
                    try
                    {
                        t.second(repeat_run);
                        retry = false;
                        if (!err)
                            ok++;
                    }
                    catch(yats_error &e)
                    {
                        err = true;
                        auto msg = make_string(ctx->name_, " :: " , t.first, ": ", e.what());
                        std::cerr << msg << std::endl; ferr << msg << std::endl;
                    }
                    catch(std::exception &e)
                    {
                        err = true; retry = false;
                        auto msg = make_string(ctx->name_, " :: " , t.first, ":\n",
                                               "    -> Unexpected exception: '", e.what(), "' error (retry interrupted).\n");
                        std::cerr << msg; ferr << msg;
                    }
                    catch(...)
                    {
                        err = true; retry = false;
                        auto msg = make_string(ctx->name_, " :: " , t.first, ":\n",
                                               "    -> Unknown exception (retry interrupted).\n");
                        std::cerr << msg; ferr << msg;
                    }
                }
                while(retry);

                if (verbose)
                    std::cout << "[" << duration_to_string(std::chrono::system_clock::now() - start) << "]" << std::endl;

                // run epilog for this test...

                for(auto & p : ctx->epilog_)
                    p.second();

                if (err && exit_immediatly)
                    _Exit(1);
            }

            // run per-group teardown tasks:

            for(auto & t : ctx->teardown_)
                t.second();
        }

        std::cerr <<  std::endl << (run-ok) << " out of " << run  << " tests failed. "
                  << global::instance().assert_ok << "/" << global::instance().assert_total << " assertions passed." << std::endl;

        return ok == run ? EXIT_SUCCESS : EXIT_FAILURE;
    }
    catch(std::exception &e)
    {
        std::cerr << "YATS: unexpected " << e.what() << " exception!" << std::endl;
        return EXIT_FAILURE;
    }

    ////////////////////////////////////////////// generic predicate:

    template <typename T>
    struct predicate
    {
        using yats_expression = void;

        predicate(const char * name, std::function<bool(const T&)> fun, const T& arg)
        : name_(name)
        , fun_(fun)
        , arg_(new T(arg))
        {}

        predicate(const char * name, std::function<bool(const T&)> fun)
        : name_(name)
        , fun_(fun)
        , arg_()
        {}

        template <typename T2>
        bool operator()(T2 value) const
        {
            return fun_(std::move(value));
        }

        std::string
        str() const
        {
            if (arg_)
                return name_ + std::string("(") + pretty(*arg_) + std::string(")");
            return name_ + "()";
        }

    private:
        std::string name_;
        std::function<bool(const T&)> fun_;
        std::shared_ptr<typename std::remove_reference<T>::type> arg_;
    };

    ////////////////////////////////////////////// yats template expressions:

    template <typename P1, typename P2, typename Fun>
    struct binary_expression
    {
        using yats_expression = void;

        binary_expression(P1 const &lhs, P2 const &rhs)
        : lhs_(lhs)
        , rhs_(rhs)
        {}

        binary_expression(binary_expression const &) = default;
        ~binary_expression() = default;

        template <typename T>
        bool operator()(T value) const
        {
            auto l = lhs_(value);
            return Fun()(l, rhs_(std::move(value)));
        }

        std::string
        str() const
        {
            return std::string("(") + lhs_.str() + std::string(" ") + Fun::str() + std::string(" ") + rhs_.str() + std::string(")");
        }

    private:
        P1 lhs_;
        P2 rhs_;
    };


    template <typename P1, typename Fun>
    struct unary_expression
    {
        using yats_expression = void;

        unary_expression(P1 const &lhs)
        : arg_(lhs)
        {}

        unary_expression(const unary_expression &) = default;
        ~unary_expression() = default;

        template <typename T>
        bool operator()(T value) const
        {
            return Fun()(arg_(std::move(value)));
        }

        std::string
        str() const
        {
            return Fun::str() + std::string("(") + arg_.str() + std::string(")");
        }

    private:
        P1 arg_;
    };

    ////////////////////////////////////////////// standard predicates:

#define YATS_FUNCTIONAL(_name_) \
    template <typename T> \
    inline predicate<T> \
    is_ ## _name_ (const T &value)  \
    {                        \
        return predicate<T>("is_" #_name_,  \
                            std::function<bool(const T&)>( \
                                std::bind(std::_name_<T>(), std::placeholders::_1, value)), \
                                value); \
    }

    YATS_FUNCTIONAL(greater)
    YATS_FUNCTIONAL(greater_equal)
    YATS_FUNCTIONAL(less)
    YATS_FUNCTIONAL(less_equal)
    YATS_FUNCTIONAL(equal_to)
    YATS_FUNCTIONAL(not_equal_to)

    ////////////////////////////////////////////// boolean combinators: or, and, not...

    struct Or : std::logical_or<bool>
    {
        static const char * str() { return "or"; }
    };

    struct And : std::logical_and<bool>
    {
        static const char * str() { return "and"; }
    };

    struct Not : std::logical_not<bool>
    {
        static const char * str() { return "not"; }
    };

    template <typename P>
    typename std::enable_if<is_yats_expression<P>::value,
    unary_expression<P, Not>>::type
    operator!(P const &expr)
    {
        return unary_expression<P, Not>(expr);
    }

    template <typename P1, typename P2>
    typename std::enable_if<is_yats_expression<P1>::value + is_yats_expression<P2>::value == 2,
    binary_expression<P1, P2, Or>>::type
    operator ||(P1 const &lhs, P2 const &rhs)
    {
        return binary_expression<P1, P2, Or>(lhs, rhs);
    }

    template <typename P1, typename P2>
    typename std::enable_if<is_yats_expression<P1>::value + is_yats_expression<P2>::value == 2,
    binary_expression<P1, P2, And>>::type
    operator &&(P1 const &lhs, P2 const &rhs)
    {
        return binary_expression<P1, P2, And>(lhs, rhs);
    }

    ////////////////////////////////////////////// boolean predicates:

    inline predicate<bool>
    is_true()
    {
        return predicate<bool>("is_boolean",
                               std::function<bool(bool)>(std::bind(std::equal_to<bool>(), std::placeholders::_1, true)), true);
    }

    inline predicate<bool>
    is_false()
    {
        return predicate<bool>("is_boolean",
                               std::function<bool(bool)>(std::bind(std::equal_to<bool>(), std::placeholders::_1, false)), false);
    }

    ////////////////////////////////////////////// predicate factory:

    template <typename Tp, typename Fn>
    inline predicate<Tp>
    make_predicate(const char *name, Fn fun)
    {
        return predicate<Tp>(name, std::function<bool(const Tp &)>(fun));
    }

    ////////////////////////////////////////////// YATS assertions:

    template <typename T, typename P>
    void assert(const char *file, int line, int id, const T &value, P pred)
    {
        if (!global::instance().yats_assert.emplace(file, line, id).second)
            return;

        global::instance().assert_total++;

        if (!pred(value)) {
                throw yats_error(file, line, make_error(file, line, "    -> predicate ", pred.str(), " failed: got ", pretty(value)));
        }

        global::instance().assert_ok++;
    }

    static inline
    void assert(const char *file, int line, int id, bool value)
    {
        return assert(file, line, id, value, is_true());
    }

    template <typename T, typename E>
    void assert_throw(const char *file, int line, int id, T const & expr, E const &obj)
    {
        if (!global::instance().yats_assert.emplace(file, line, id).second)
            return;

        global::instance().assert_total++;

        try
        {
            expr();
        }
        catch(E &e)
        {
            if (std::string(e.what()).compare(obj.what()))
                throw yats_error(file, line, make_error(file, line,
                                             "    -> ", yats::type_name(obj),
                                             " exception caught with reason \"",
                                             e.what(),
                                             "\" != \"", obj.what(), "\"!"));
            global::instance().assert_ok++;
            return;
        }
        catch(std::exception &e)
        {
            if (!std::is_same<E,anything>::value)
                throw yats_error(file, line, make_error(file, line,
                                            "    -> ", yats::type_name(obj),
                                            " exception expected. Got ",
                                            yats::type_name(e),
                                            " (\"", e.what(), "\")!"));
            global::instance().assert_ok++;
            return;
        }
        catch(...)
        {
            if (!std::is_same<E,anything>::value)
                throw yats_error(file, line, make_error(file, line,
                                            "    -> ", yats::type_name(obj),
                                            " exception expected: got unknown exception!"));
            global::instance().assert_ok++;
            return;
        }

        if (!std::is_same<E, nothing>::value)
            throw yats_error(file, line, make_error(file, line,
                                        "    -> ", yats::type_name(obj),
                                        " exception expected!"));

        global::instance().assert_ok++;
    }

} // namespace yats

#endif /* _YATS_HPP_ */
