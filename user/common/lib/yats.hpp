/*
 *  Copyright (c) 2011-2012 Bonelli Nicola <bonelli@antifork.org>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 */


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
#include <cstdlib>
#include <string>
#include <cstring>
#include <type_traits>
#include <functional>
#include <algorithm>
#include <stdexcept>
#include <typeinfo>
#include <vector>
#include <memory>
#include <map>
#include <set>
#include <random>
#include <chrono>
#include <csignal>

#ifdef __GNUC__
#include <cxxabi.h>
#endif


/////////////////////////////////////////////////////////////////////////////////////////////////


#define Assert(...)             YATS_ASSERT(__VA_ARGS__)
#define AssertNoThrow(...)      YATS_ASSERT_NOTHROW(__VA_ARGS__)
#define AssertThrow(...)        YATS_ASSERT_THROW(__VA_ARGS__)
#define AssertThrowAs(...)      YATS_ASSERT_THROW_AS(__VA_ARGS__)
#define StaticError(expr,msg)   YATS_XPASTE(YATS_STATIC_ERROR_, __COUNTER__) (expr,msg)


#define Context(ctx) \
    namespace ctx { static const char _context_name[] = #ctx; } \
    namespace ctx


#define Test(name) \
    void test_ ## name(const char *); \
    yats::task_register hook_ ## name(test_ ## name, yats::task_register::type::test, _context_name, #name); \
    void test_ ## name(const char *_test_name __attribute__((unused)))


#define Setup(name) \
    void setup_ ## name(const char *); \
    yats::task_register fixture_ ## name(setup_ ## name, yats::task_register::type::setup, _context_name); \
    void setup_ ## name(const char *)


#define Teardown(name) \
    void teardown_ ## name(const char *); \
    yats::task_register fixture_ ## name(teardown_ ## name, yats::task_register::type::teardown, _context_name); \
    void teardown_ ## name(const char *)


#define Random(name, ...) \
    void random_ ## name(const char *, YATS_FOR_EACH(YATS_DIST_RES_TYPE,__VA_ARGS__)); \
    yats::task_register rhook_ ## name(yats::extended_tag(), (yats::RandTask<decltype(RandomEngine), YATS_FOR_EACH(YATS_DIST_TYPE, __VA_ARGS__)>\
                                                              (random_ ## name, RandomEngine, YATS_FOR_EACH(YATS_DIST_INSTANCE, __VA_ARGS__))), \
                                         yats::task_register::type::random, _context_name, #name); \
    void random_ ## name(const char *_test_name __attribute__((unused)), YATS_FOR_EACH(YATS_DIST_RES_ARGT,__VA_ARGS__))



/////////////////////////////////////////////////////////////////////////////////////////////////


#define YATS_ASSERT(...)                yats::assert        (__FILE__, __LINE__, _context_name, _test_name, __VA_ARGS__)
#define YATS_ASSERT_NOTHROW(...)        yats::assert_throw  (__FILE__, __LINE__, _context_name, _test_name, [&](){ __VA_ARGS__; }, nothing())
#define YATS_ASSERT_THROW(...)          yats::assert_throw  (__FILE__, __LINE__, _context_name, _test_name, [&](){ __VA_ARGS__; }, anything())
#define YATS_ASSERT_THROW_AS(e, ...)    yats::assert_throw  (__FILE__, __LINE__, _context_name, _test_name, [&](){ __VA_ARGS__; }, e)


/* the so-called __VA_NARG__ (PP_NARG) macro from the thread at
   http://groups.google.com/group/comp.std.c/browse_frm/thread/77ee8c8f92e4a3fb
 */

#define YATS_PP_NARG(...) \
         YATS_PP_NARG_(__VA_ARGS__,YATS_PP_RSEQ_N())
#define YATS_PP_NARG_(...) \
         YATS_PP_ARG_N(__VA_ARGS__)
#define YATS_PP_ARG_N( \
          _1, _2, _3, _4, _5, _6, _7, _8, _9,_10, \
         _11,_12,_13,_14,_15,_16,_17,_18,_19,_20, \
         _21,_22,_23,_24,_25,_26,_27,_28,_29,_30, \
         _31,_32,_33,_34,_35,_36,_37,_38,_39,_40, \
         _41,_42,_43,_44,_45,_46,_47,_48,_49,_50, \
         _51,_52,_53,_54,_55,_56,_57,_58,_59,_60, \
         _61,_62,_63,N,...) N

#define YATS_PP_RSEQ_N() \
         63,62,61,60,                   \
         59,58,57,56,55,54,53,52,51,50, \
         49,48,47,46,45,44,43,42,41,40, \
         39,38,37,36,35,34,33,32,31,30, \
         29,28,27,26,25,24,23,22,21,20, \
         19,18,17,16,15,14,13,12,11,10, \
         9,8,7,6,5,4,3,2,1,0

#define YATS_PASTE(a,b)         a ## b
#define YATS_XPASTE(a,b)        YATS_PASTE(a,b)

#define YATS_UNPACK_ARGS(...)   __VA_ARGS__
#define YATS_UNPACK(x)          YATS_UNPACK_ARGS x
#define YATS_APPLY(f, ...)      f(__VA_ARGS__)

#define YATS_FOR_EACH_1(f, x)        YATS_APPLY(f,YATS_UNPACK(x))
#define YATS_FOR_EACH_2(f, x, ...)   YATS_APPLY(f,YATS_UNPACK(x)) , YATS_FOR_EACH_1(f, __VA_ARGS__)
#define YATS_FOR_EACH_3(f, x, ...)   YATS_APPLY(f,YATS_UNPACK(x)) , YATS_FOR_EACH_2(f, __VA_ARGS__)
#define YATS_FOR_EACH_4(f, x, ...)   YATS_APPLY(f,YATS_UNPACK(x)) , YATS_FOR_EACH_3(f, __VA_ARGS__)
#define YATS_FOR_EACH_5(f, x, ...)   YATS_APPLY(f,YATS_UNPACK(x)) , YATS_FOR_EACH_4(f, __VA_ARGS__)
#define YATS_FOR_EACH_6(f, x, ...)   YATS_APPLY(f,YATS_UNPACK(x)) , YATS_FOR_EACH_5(f, __VA_ARGS__)
#define YATS_FOR_EACH_7(f, x, ...)   YATS_APPLY(f,YATS_UNPACK(x)) , YATS_FOR_EACH_6(f, __VA_ARGS__)
#define YATS_FOR_EACH_8(f, x, ...)   YATS_APPLY(f,YATS_UNPACK(x)) , YATS_FOR_EACH_7(f, __VA_ARGS__)
#define YATS_FOR_EACH_9(f, x, ...)   YATS_APPLY(f,YATS_UNPACK(x)) , YATS_FOR_EACH_8(f, __VA_ARGS__)
#define YATS_FOR_EACH_10(f, x, ...)  YATS_APPLY(f,YATS_UNPACK(x)) , YATS_FOR_EACH_9(f, __VA_ARGS__)
#define YATS_FOR_EACH(f, ...)        YATS_XPASTE(YATS_FOR_EACH_, YATS_PP_NARG(__VA_ARGS__))(f, __VA_ARGS__)

#define YATS_DIST_TYPE(dist, name, ...)        dist
#define YATS_DIST_ARG_NAME(dist, name, ...)    name
#define YATS_DIST_RES_TYPE(dist, name, ...)    dist::result_type
#define YATS_DIST_RES_ARGT(dist, name, ...)    dist::result_type name
#define YATS_DIST_INSTANCE(dist, name, ...)    dist(__VA_ARGS__)

#define YATS_HEADER(ctx,test,file,line) file, ':', line, ": *** ", ctx, "::", test, ":\n"

#define YATS_STATIC_ERROR_CLASS(expr, msg) \
    struct static_error {\
        static_error() \
        { expr; \
            std::cerr << "Static error failure: test " # expr ": " msg " is falsifiable." << std::endl; \
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

    ////////////////////////////////////////////// C++ yats exception:

    struct yats_error : public std::runtime_error
    {
        explicit yats_error(const std::string &msg)
        : std::runtime_error(msg)
        { }

        yats_error(const yats_error &) = default;

        virtual ~yats_error() noexcept
        { }
    };

    ////////////////////////////////////////////// nothing and anything:

    struct nothing
    {
        static constexpr const char * what()
        {
            return "nothing";
        }
    };

    inline std::string
    type_name(nothing)
    {
        return "no";
    }

    struct anything
    {
        static constexpr const char * what()
        {
            return "anything";
        }
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
        int status;
        std::unique_ptr<char, void (*)(void *)> ret(abi::__cxa_demangle(name,0,0, &status), ::free);
        if (status < 0)
            throw yats_error("__cxa_demangle");
        return std::string(ret.get());
    }

    template <typename Tp>
    inline std::string
    type_name(const Tp &t)
    {
        return cxa_demangle(typeid(t).name());
    }


    ////////////////////////////////////////////// seq and gens metafunction:

    // http://stackoverflow.com/questions/7858817/unpacking-a-tuple-to-call-a-matching-function-pointer


    template<int ...>
    struct seq { };

    template<int N, int ...S>
    struct gens : gens<N-1, N-1, S...> { };

    template<int ...S>
    struct gens<0, S...> {
        typedef seq<S...> type;
    };


    ////////////////////////////////////////////// RandTask callable object:

    template <typename Engine, typename ... Dist>
    struct RandTask
    {
        typedef void (*function_t)(const char *name, typename Dist::result_type...);
        typedef void result_type;

        RandTask(function_t f, Engine &e, Dist ... ds)
        : fun_(f)
        , engine_(e)
        , distributions_(std::move(ds)...)
        {}

        template <int ...S>
        void call_test(const char *name, seq<S...>)
        {
            fun_(name, std::get<S>(distributions_)(engine_)...);
        }

        void operator()(const char *name, int run)
        {
            for(int i = 0; i < run; i++)
            {
                call_test(name, typename gens<sizeof...(Dist)>::type());
            }
        }

    private:

        function_t fun_;
        Engine &engine_;
        std::tuple<Dist...> distributions_;
    };


    ////////////////////////////////////////////// YATS contexts:

    struct context
    {
        typedef std::function<void(int)> Task;

        std::string name_;

        std::vector<Task> setup_;
        std::vector<Task> teardown_;
        std::vector<std::pair<Task,std::string>> task_list_;

    public:
        static std::map<std::string, std::shared_ptr<context>> &
        instance()
        {
            static std::map<std::string, std::shared_ptr<context>> m;
            return m;
        }

        context(const std::string &n)
        : name_(n)
        , setup_()
        , teardown_()
        , task_list_()
        {}

        context(context const &) = delete;
        context& operator=(context const &) = delete;
    };


    ////////////////////////////////////////////// usage:

    static void usage(const char *name)
    {
        std::cout << "Yats usage: " << name << " [options] [test...]" << std::endl;
        std::cout << "Options:\n";
        std::cout << "  -e, --exit-immediately  On error exit.\n";
        std::cout << "  -c, --context context   Run tests from the given context.\n";
        std::cout << "  -v, --verbose           Verbose mode.\n";
        std::cout << "  -s, --signal            Capture unix signals.\n";
        std::cout << "  -r, --run int           Number of run per Random test (1000 default).\n";
        std::cout << "  -l, --list              Print the list of tests\n";
        std::cout << "  -h, --help              Print this help.\n";

        _Exit(EXIT_SUCCESS);
    }


    ////////////////////////////////////////////// print:

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


    ////////////////////////////////////////////// singleton:

    struct singleton
    {
        static uint32_t &
        assert_counter()
        {
            static uint32_t c;
            return c;
        }

        static const std::string &
        program_name(std::string n = "")
        {
            static std::string name(n);
            return name;
        }

        static const std::map<int, std::string> &
        unix_signal_map()
        {
            static std::map<int, std::string> m = []() {
                std::map<std::string, std::vector<int>> reverse
                    = {{"SIGHUP"  ,  {     1     }},
                       {"SIGINT"  ,  {     2     }},
                       {"SIGQUIT" ,  {     3     }},
                       {"SIGILL"  ,  {     4     }},
                       {"SIGABRT" ,  {     6     }},
                       {"SIGFPE"  ,  {     8     }},
                       {"SIGKILL" ,  {     9     }},
                       {"SIGSEGV" ,  {    11     }},
                       {"SIGPIPE" ,  {    13     }},
                       {"SIGALRM" ,  {    14     }},
                       {"SIGTERM" ,  {    15     }},
                       {"SIGUSR1" ,  { 30,10,16  }},
                       {"SIGUSR2" ,  { 31,12,17  }},
                       {"SIGCHLD" ,  { 20,17,18  }},
                       {"SIGCONT" ,  { 19,18,25  }},
                       {"SIGSTOP" ,  { 17,19,23  }},
                       {"SIGTSTP" ,  { 18,20,24  }},
                       {"SIGTTIN" ,  { 21,21,26  }},
                       {"SIGTTOU" ,  { 22,22,27  }},
                       {"SIGBUS"  ,  { 10,7,10   }},
                       {"SIGPROF" ,  { 27,27,29  }},
                       {"SIGSYS"  ,  { 12,31,12  }},
                       {"SIGTRAP" ,  {    5      }},
                       {"SIGURG"  ,  { 16,23,21  }},
                       {"SIGVTALRM", { 26,26,28  }},
                       {"SIGXCPU" ,  { 24,24,30  }},
                       {"SIGXFSZ" ,  { 25,25,31  }}};

                std::map<int, std::string> sigmap;

                for (auto const & sig : reverse)
                {
                    for(auto const &signum : sig.second)
                    {
                        sigmap.insert(std::make_pair(signum, sig.first));
                    }
                }

                return sigmap;
            }();

            return m;
        }

        static std::string
        unix_signal(int n)
        {
            auto it = unix_signal_map().find(n);
            if (it != std::end(unix_signal_map())) {
                return it->second;
            }
            return std::string("SIGNUM ") + std::to_string(n);
        }
    };


    ////////////////////////////////////////////// run tests:

    static void sig_handler(int n)
    {
        std::ofstream ferr("/tmp/" + singleton::program_name(), std::fstream::app);
        ferr << singleton::unix_signal(n) << std::endl;
        _Exit (-n);
    }

    static int run(int argc = 0, char *argv[] = nullptr)
    {
        bool exit_immediatly = false,
             err             = false,
             verbose         = false,
             capture_signal  = false;
        int  repeat_run      = 1000;

        std::set<std::string> run_ctx, run_test;

        singleton::program_name(argv[0]);

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

            if (strcmp(*arg, "-c") == 0 ||
                strcmp(*arg, "--context") == 0) {
                if (++arg == (argv+argc))
                    throw std::runtime_error("YATS: number of repetition missing");
                run_ctx.insert(*arg);
                continue;
            }

            if (strcmp(*arg, "-r") == 0 ||
                strcmp(*arg, "--run") == 0) {
                if (++arg == (argv+argc))
                    throw std::runtime_error("YATS: context missing");
                repeat_run = atoi(*arg);
                continue;
            }

            if (strcmp(*arg, "-l") == 0 ||
                strcmp(*arg, "--list") == 0) {

                for(auto & c : context::instance())
                {
                    std::cout << "Context " << c.first << ": ";
                    int n = 1;
                    for(auto & t : c.second->task_list_)
                    {
                        std::cout << t.second << ' ';
                        if ((n++ & 7) == 0)
                            std::cout << std::endl << "    ";
                    }
                    std::cout << std::endl;
                }
                _Exit(1);
            }

            run_test.insert(*arg);
        }

        std::cout << "YATS: verbose " << std::boolalpha << verbose << ", UNIX signals " << capture_signal << std::endl;

        if (capture_signal) {
            for(int n = 0; n < 64; n++)
                signal(n,  sig_handler);
        }

        size_t tot_ctx = 0, tot_task = 0;

        for(auto & ctx : context::instance())
        {
            if (!run_ctx.empty() && run_ctx.find(ctx.first) == run_ctx.end())
                continue;

            tot_ctx++;
            tot_task += [&]() -> size_t {

                        if (run_test.empty())
                            return ctx.second->task_list_.size();
                        else
                            return static_cast<size_t>(
                                std::count_if (std::begin(ctx.second->task_list_),
                                       std::end(ctx.second->task_list_),
                                       [&] (std::pair<context::Task,std::string> const &elem) -> bool {

                                            return std::find(std::begin(run_test), std::end(run_test), elem.second) != std::end(run_test);
                                       }));
                        }();
        }

        unsigned int run = 0, ok = 0;

        std::ofstream ferr("/tmp/" + singleton::program_name());

        std::cout << "Loading " << tot_task << " tests in " << tot_ctx << " contexts." << std::endl;

        // iterate over contexts:
        //

        for(auto & c : context::instance())
        {
            if (!run_ctx.empty() && run_ctx.find(c.first) == run_ctx.end())
                continue;

            if (verbose)
                std::cout << "Context " << c.first << ":\n";

            // run setup:
            //

            for(auto & t : c.second->setup_)
            {
                t(0);
            }

            // for each task...
            //

            for(auto& t : c.second->task_list_)
            {
                if (!run_test.empty() &&
                    run_test.find(t.second) == run_test.end())
                    continue;

                if (verbose)
                    std::cout << "+ running '" << t.second << "'... " << std::flush;

                run++;

                auto start = std::chrono::system_clock::now();

                try
                {
                    // run the test here
                    //
                    t.first.operator()(repeat_run);
                    ok++;
                }
                catch(yats_error &e)
                {
                    err = true;
                    std::cerr << e.what() << std::endl;
                    ferr      << e.what() << std::endl;
                }
                catch(std::exception &e)
                {
                    err = true;
                    auto msg = make_string("test ", c.first, "::" , t.second , ":\n", "    -> Unexpected exception: '", e.what(), "' error.\n");
                    std::cerr << msg;
                    ferr      << msg;
                }
                catch(...)
                {
                    err = true;

                    auto msg = make_string("test ", c.first, "::" , t.second , ":\n", "    -> Unknown exception.\n");
                    std::cerr << msg;
                    ferr      << msg;
                }

                if (err && exit_immediatly)
                    _Exit(1);

                if (verbose)
                    std::cout << "[" << duration_to_string(std::chrono::system_clock::now() - start) << "]" << std::endl;

            }

            // run teardown:
            //

            for(auto & t : c.second->teardown_)
            {
                t(0);
            }

        }

        std::cerr <<  std::endl << (run-ok) << " out of " << run  << " tests failed. " << singleton::assert_counter() << " assertions passed." << std::endl;

        return ok == run ? EXIT_SUCCESS : EXIT_FAILURE;
    }


    ////////////////////////////////////////////// task register tool:

    struct extended_tag {};

    struct task_register
    {
        enum class type { test, random, setup, teardown };

        template <typename Fn>
        task_register(Fn fun, type t, const char * ctx, const char *name= "")
        {
            auto it = context::instance().find(ctx);
            if (it == context::instance().end())
            {
                std::shared_ptr<context> nc(new context(ctx));
                it = context::instance().insert(std::make_pair(ctx, std::move(nc))).first;
            }

            switch(t)
            {
            case type::test:
                it->second->task_list_.push_back(std::make_pair(context::Task{std::bind(fun, name)}, name));
                break;
            case type::setup:
                it->second->setup_.push_back(context::Task{std::bind(fun,name)});
                break;
            case type::teardown:
                it->second->teardown_.push_back(context::Task{std::bind(fun,name)});
                break;
            case type::random:
                throw std::logic_error("yats");
            }
        }

        template <typename Fn>
        task_register(extended_tag, Fn fun, type t, const char * ctx, const char *name= "")
        {
            auto it = context::instance().find(ctx);
            if (it == context::instance().end())
            {
                std::shared_ptr<context> nc(new context(ctx));
                it = context::instance().insert(std::make_pair(ctx, std::move(nc))).first;
            }

            switch(t)
            {
            case type::random:
                it->second->task_list_.push_back(std::make_pair(std::bind(fun, name, std::placeholders::_1), name));
                break;
            default:
                throw std::logic_error("yats");
            }
        }
    };


    ////////////////////////////////////////////// magic sfinae:
    // ...for has_insertion_operator

    struct __sfinae_types
    {
      typedef char __one;
      typedef struct { char __arr[2]; } __two;
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
            o << ':' << std::hex << "0x" << v << std::dec;
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

    ////////////////////////////////////////////// generic predicate:

    template <typename T>
    struct predicate
    {
        typedef int yats_expression;

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

        bool
        operator()(T const& value) const
        {
            return fun_(value);
        }

        template <typename T2>
        bool
        operator()(T2 value) const
        {
            return fun_(std::move(value));
        }

        std::string
        str() const
        {
            if (arg_)
                return name_ + std::string("(") + pretty(*arg_) + std::string(")");
            else
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
        typedef int yats_expression;

    private:
        P1 lhs_;
        P2 rhs_;

    public:
        binary_expression(P1 const &lhs, P2 const &rhs)
        : lhs_(lhs)
        , rhs_(rhs)
        {}

        binary_expression(binary_expression const &) = default;

        ~binary_expression() = default;

        template <typename T>
        bool
        operator()(T value) const
        {
            auto l = lhs_(value);
            return Fun()(l, rhs_(std::move(value)));
        }

        std::string
        str() const
        {
            return std::string("(") + lhs_.str() +
                   std::string(" ") + Fun::str() + std::string(" ") + rhs_.str() + std::string(")");
        }
    };


    template <typename P1, typename Fun>
    struct unary_expression
    {
        typedef int yats_expression;

    private:
        P1 arg_;

    public:
        unary_expression(P1 const &lhs)
        : arg_(lhs)
        {}

        unary_expression(const unary_expression &) = default;

        ~unary_expression() = default;

        template <typename T>
        bool
        operator()(T value) const
        {
            return Fun()(arg_(std::move(value)));
        }

        std::string
        str() const
        {
            return Fun::str() + std::string("(") + arg_.str() + std::string(")");
        }
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
        return predicate<bool>( "is_boolean",
                                std::function<bool(bool)>(
                                    std::bind(std::equal_to<bool>(), std::placeholders::_1, true)), true);
    }

    inline predicate<bool>
    is_false()
    {
        return predicate<bool>( "is_boolean",
                                std::function<bool(bool)>(
                                    std::bind(std::equal_to<bool>(), std::placeholders::_1, false)), false);
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
    void assert(const char *file, int line, const char *ctx, const char *test, const T &value, P pred)
    {
        if (!pred(value))
        {
            throw yats_error(make_string(YATS_HEADER(ctx, test, file, line),
                             "    -> predicate ", pred.str(), " failed: got ", pretty(value)));
        }

        singleton::assert_counter()++;
    }

    static inline
    void assert(const char *file, int line, const char *ctx, const char *test, bool value)
    {
        return assert(file, line, ctx, test, value, is_true());
    }

    template <typename T, typename E>
    void assert_throw(const char *file, int line, const char *ctx, const char *test, T const & expr, E const &obj)
    {
        try
        {
            expr();
        }
        catch(E &e)
        {
            if (std::string(e.what()).compare(obj.what()))
                throw yats_error(make_string(YATS_HEADER(ctx, test, file, line),
                                             "    -> ", yats::type_name(obj),
                                             " exception caught with reason \"",
                                             e.what(),
                                             "\" != \"", obj.what(), "\"!"));
            singleton::assert_counter()++;
            return;
        }
        catch(std::exception &e)
        {
            if (!std::is_same<E,anything>::value)
                throw yats_error(make_string(YATS_HEADER(ctx, test, file, line),
                                            "    -> ", yats::type_name(obj),
                                            " exception expected. Got ",
                                            yats::type_name(e),
                                            " (\"", e.what(), "\")!"));
            singleton::assert_counter()++;
            return;
        }
        catch(...)
        {
            if (!std::is_same<E,anything>::value)
                throw yats_error(make_string(YATS_HEADER(ctx, test, file, line),
                                            "    -> ", yats::type_name(obj),
                                            " exception expected: got unknown exception!"));
            singleton::assert_counter()++;
            return;
        }

        if (!std::is_same<E, nothing>::value)
            throw yats_error(make_string(YATS_HEADER(ctx, test, file, line),
                                        "    -> ", yats::type_name(obj),
                                        " exception expected!"));

        singleton::assert_counter()++;

    }

}

#endif /* _YATS_HPP_ */
