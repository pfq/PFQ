/*
 *  Copyright (c) 2011 Bonelli Nicola <bonelli@antifork.org>
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
 
#ifndef _YATS_HPP_
#define _YATS_HPP_ 

#include <cstdlib>
#include <iostream>
#include <sstream>
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

#ifdef __GNUC__
#include <cxxabi.h>
#endif

/* the so-called __VA_NARG__ (PP_NARG) macro from the thread at 
   http://groups.google.com/group/comp.std.c/browse_frm/thread/77ee8c8f92e4a3fb 
 */

#ifndef PP_NARG
#define PP_NARG(...) \
         PP_NARG_(__VA_ARGS__,PP_RSEQ_N())
#define PP_NARG_(...) \
         PP_ARG_N(__VA_ARGS__)
#define PP_ARG_N( \
          _1, _2, _3, _4, _5, _6, _7, _8, _9,_10, \
         _11,_12,_13,_14,_15,_16,_17,_18,_19,_20, \
         _21,_22,_23,_24,_25,_26,_27,_28,_29,_30, \
         _31,_32,_33,_34,_35,_36,_37,_38,_39,_40, \
         _41,_42,_43,_44,_45,_46,_47,_48,_49,_50, \
         _51,_52,_53,_54,_55,_56,_57,_58,_59,_60, \
         _61,_62,_63,N,...) N
#define PP_RSEQ_N() \
         63,62,61,60,                   \
         59,58,57,56,55,54,53,52,51,50, \
         49,48,47,46,45,44,43,42,41,40, \
         39,38,37,36,35,34,33,32,31,30, \
         29,28,27,26,25,24,23,22,21,20, \
         19,18,17,16,15,14,13,12,11,10, \
         9,8,7,6,5,4,3,2,1,0 
#endif

#ifndef PASTE
#define PASTE(a,b)      a ## b
#define XPASTE(a,b)     PASTE(a,b)
#endif

struct yats_error : public std::runtime_error
{
    explicit yats_error(const std::string &msg)
    : std::runtime_error(msg)
    {}
    
    virtual ~yats_error() throw() 
    {}
};

#define YATS_ASSERT_1(value)            yats::assert_predicate(value, is_true(),_context_name, _name, __LINE__)
#define YATS_ASSERT_2(value,pred)       yats::assert_predicate(value, pred,     _context_name, _name, __LINE__)

#define YATS_ASSERT_THROW_1(value)      YATS_ASSERT_THROW_ANY (value, __LINE__)
#define YATS_ASSERT_THROW_2(value,obj)  YATS_ASSERT_THROW_TYPE(value, obj, __LINE__)

#define YATS_ERROR(context, name) "Context " << context << ": Test(" << name << ")\n"  

#define YATS_ASSERT_NOTHROW(exp,line) \
try \
{ \
    exp; \
} \
catch(std::exception &e) \
{           \
    std::ostringstream err; \
    err << std::boolalpha << YATS_ERROR(_context_name, _name) \
                        << " -> exception not expected. Got " \
                        << yats::type_name(e) << "(\"" << e.what() << "\")" \
                        << " error at line " << line; \
    throw yats_error(err.str()); \
} \
catch(...) \
{           \
    std::ostringstream err; \
    err << std::boolalpha << YATS_ERROR(_context_name, _name) \
                        << " -> exception not expected: got unknown exception. Error at line " << line; \
    throw yats_error(err.str()); \
} 

#define YATS_ASSERT_THROW_ANY(exp,line) \
{ \
    bool thrown = false; \
    try \
    { \
        exp; \
    } \
    catch(...) \
    {                \
        thrown = true; \
    }           \
    if (!thrown) \
    {  \
        std::ostringstream e; \
        e << std::boolalpha << YATS_ERROR(_context_name, _name) \
                            << " -> exception expected. Error at line " << line; \
        throw yats_error(e.str()); \
    }  \
}

#define YATS_ASSERT_THROW_TYPE(exp, obj, line) \
{ \
    bool thrown = false; \
    try \
    { \
        exp; \
    } \
    catch(decltype(obj) &e) \
    { \
        if (std::string(e.what()).compare(obj.what())) { \
            std::ostringstream err; \
            err << std::boolalpha << YATS_ERROR(_context_name, _name) \
                            << " -> " << yats::type_name(obj)  \
                            <<  " caught with reason '" << e.what() << "' != '" << obj.what()  << "'. Error at line " << line; \
            throw yats_error(err.str()); \
        } \
        thrown = true; \
    } \
    catch(...) \
    { \
        std::ostringstream err; \
        err << std::boolalpha << YATS_ERROR(_context_name, _name) \
                        << " -> " << yats::type_name(obj)  \
                        <<  " expected. Got unknown exception. Error at line " << line; \
        throw yats_error(err.str()); \
    }  \
    \
    if (!thrown) \
    {  \
        std::ostringstream err; \
        err << std::boolalpha << YATS_ERROR(_context_name, _name) \
                            << " -> exception " << yats::type_name(obj) << " expected. Error at line " << line; \
        throw yats_error(err.str()); \
    }  \
}

///////////////////////////////////////////////////////////////////////////////////////////////////////

#define YATS_STATIC_ERROR_CLASS(expr, msg) \
    struct static_error {\
        static_error() \
        { expr; \
            std::cerr << "Static error failure: Test(" # expr "): " msg " is falsifiable." << std::endl; \
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

///////////////////////////////////////////////////////////////////////////////////////////////////////

using namespace std::placeholders;

namespace yats 
{
#ifdef __GNUC__
    static inline
    std::string
    cxa_demangle(const char *name)
    {
        int status;
        std::unique_ptr<char, void (*)(void *)> ret(abi::__cxa_demangle(name,0,0, &status), ::free);
        if (status < 0) 
            throw yats_error("__cxa_demangle");
        return std::string(ret.get());
    }
#else
    static inline
    std::string
    cxa_demangle(const char *name)
    {
        return std::string(name);
    }
#endif

    template <typename Tp>
    std::string
    type_name(const Tp &t)
    {
        return cxa_demangle(typeid(t).name());
    }
    
    struct context
    {
        typedef std::function<void()> task;

        std::string name_;
        std::vector<task> setup_;
        std::vector<task> teardown_;
        std::vector<std::pair<task,std::string>> task_list_;
        
        static std::map<std::string, context> &
        instance()
        {
            static std::map<std::string, context> m;
            return m;
        }
        
        context(const std::string &n)
        : name_(n), setup_(), teardown_(), task_list_()
        {}
    };

    static void usage(const char *name)
    {
        std::cout << "Yats usage: " << name << " [options] [test...]" << std::endl;
        std::cout << "Options:\n";
        std::cout << "  -e, --exit-immediatly   On error exit.\n";
        std::cout << "  -c, --context context   Run tests from the given context.\n";
        std::cout << "  -v, --verbose           Verbose mode.\n";
        std::cout << "  -h, --help              Print this help.\n";

        _Exit(EXIT_SUCCESS);
    }

    static int run(int argc = 0, char *argv[] = nullptr)
    {
        bool exit_immediatly = false, err = false;
        bool verbose = false;

        std::set<std::string> cxt;
        std::set<std::string> test;

        for(auto arg = argv + 1; argv && (arg != argv + argc); ++arg)
        {
            if (strcmp(*arg, "-h") == 0 ||
                strcmp(*arg, "--help") == 0) {
                usage(argv[0]);
            }

            if (strcmp(*arg, "-e") == 0 ||
                strcmp(*arg, "--exit-immediatly") == 0) {
                exit_immediatly = true;
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
                    throw std::runtime_error("YATS: context missing");
                cxt.insert(*arg);
                continue;
            }

            test.insert(*arg);
        }

        size_t tot_task = 0;
        for(auto& task : context::instance())
        {
            if (!cxt.empty() &&
                cxt.find(task.first) == cxt.end())
                continue;
            tot_task += task.second.task_list_.size();
        }

        unsigned int run = 0, ok = 0;
        std::cout << "Loading " << tot_task << " tests in " << context::instance().size() << " contexts." << std::endl;

        // iterate over contexts: 
        //
        for(auto& c : context::instance()) 
        {
            if (!cxt.empty() &&
                cxt.find(c.first) == cxt.end())
                continue;

            if (verbose)
                std::cout << "Context " << c.first << ":\n";

            // run setup:
            std::for_each(std::begin(c.second.setup_), 
                          std::end(c.second.setup_), 
                          std::mem_fn(&context::task::operator()));

            // for each task... 
            for(auto& t : c.second.task_list_)
            {
                if (!test.empty() &&
                    test.find(t.second) == test.end())
                    continue;

                if (verbose)
                    std::cout << "+ running Test(" << t.second << ")...\n";
                
                run++;
                try
                {    
                    // run the test...
                    t.first.operator()();
                    ok++;  
                }   
                catch(yats_error &e)
                {
                    err = true;
                    std::cerr << e.what() << std::endl;
                }
                catch(std::exception &e)
                {
                    err = true;
                    std::cerr << "Context " << c.first << ": Test(" << t.second << ")\n -> Unexpected exception: '" << e.what() << "' error." << std::endl;
                }
                
                if (err && exit_immediatly)
                    _Exit(1);
            }
            
            // run teardown:
            std::for_each( std::begin(c.second.teardown_), 
                           std::end(c.second.teardown_), 
                           std::mem_fn(&context::task::operator()));
        }

        std::cerr << (run-ok) << " out of " << run  << " tests failed." << std::endl;
        return ok == run ? EXIT_SUCCESS : EXIT_FAILURE;
    }

    struct task_register
    {
        enum class type { test, setup, teardown };

        template <typename Fn>
        task_register(Fn fun, type t, const char * ctx, const char *name= "")
        {
            auto i = context::instance().insert(std::make_pair(ctx, context(ctx)));

            switch(t)
            {
            case type::test:    
                i.first->second.task_list_.push_back(std::make_pair(std::bind(fun, name), name));        
                break;
            case type::setup:
                i.first->second.setup_.push_back(std::bind(fun,name)); 
                break;
            case type::teardown:
                i.first->second.teardown_.push_back(std::bind(fun,name)); 
                break;
            default:
                throw yats_error("task_register");
            }
        }
    };

    // For use in __is_convertible_simple.
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
    typename std::enable_if<std::is_integral<T>::value, std::string>::type
    pretty_value(const T &v)
    {
        std::ostringstream o;
        o << std::boolalpha << '(' << v; 
        if (v > 15) 
            o << std::hex << "=0x" << v;
        o << ')';
        return o.str();
    }
    template <typename T>
    typename std::enable_if<!std::is_integral<T>::value &&
                            has_insertion_operator<T>::value, std::string>::type
    pretty_value(const T &v)
    {
        std::ostringstream o;
        o << '(' << v << ')';
        return o.str();
    }
    template <typename T>
    typename std::enable_if<!std::is_integral<T>::value &&
                            !has_insertion_operator<T>::value, std::string>::type
    pretty_value(const T &)
    {
        return "()";
    }

    ///////////////////// generic predicate ///////////////////////////

    template <typename T>
    struct predicate
    {
        predicate(const char * name, std::function<bool(const T&)> fun, const T& arg)
        : name_(name)
        , fun_(fun)
        , arg_(std::make_pair(arg, true))
        {}
        
        predicate(const char * name, std::function<bool(const T&)> fun)
        : name_(name)
        , fun_(fun)
        , arg_()
        {}
        
        bool 
        operator()(const T &value) const
        {
            return fun_(value);
        }

        bool
        has_arg() const
        {
            return arg_.second;
        }

        const T &
        arg() const
        {
            return arg_.first;
        }

        const
        std::string &
        name() const
        {
            return name_;
        }

    private:
        std::string name_;
        std::function<bool(const T&)> fun_;
        std::pair<typename std::remove_reference<T>::type, bool> arg_;
    };

    template <typename T1, typename T2>
    void assert_predicate(const T1 &value, const predicate<T2> &pred, const char *ctx, const char *name, int line)
    {
        if (!pred(value)) {
            std::ostringstream err;
            err << std::boolalpha << YATS_ERROR(ctx, name) 
                                << " -> predicate " << pred.name(); 
            
            if (pred.has_arg())
                err << pretty_value(pred.arg()); 
            err << " failed: got " << pretty_value(value) <<  ". Error at line " << line;
            throw yats_error(err.str());
        }
    }

    /// standard predicates...

#define YATS_FUNCTIONAL(_name_) \
    template <typename T> \
    inline predicate<T> \
    is_ ## _name_ (const T &value)  \
    {                        \
        return predicate<T>("is_" #_name_,  \
                            std::function<bool(const T&)>( \
                                std::bind(std::_name_<T>(), _1, value)), \
                                value); \
    }

    YATS_FUNCTIONAL(greater);
    YATS_FUNCTIONAL(greater_equal);
    YATS_FUNCTIONAL(less);
    YATS_FUNCTIONAL(less_equal);
    YATS_FUNCTIONAL(equal_to);
    YATS_FUNCTIONAL(not_equal_to);

    /// boolean...

    inline predicate<bool>
    is_true()
    {
        return predicate<bool>("is_boolean",  
                            std::function<bool(bool)>(
                                std::bind(std::equal_to<bool>(), _1, true)), true); 
    }
    
    inline predicate<bool>
    is_false()
    {
        return predicate<bool>("is_boolean",  
                            std::function<bool(bool)>(
                                std::bind(std::equal_to<bool>(), _1, false)), false); 
    }

    /// generic predicate: bool(Tp)

    template <typename Tp, typename Fn>
    inline predicate<Tp>
    generic_predicate(const char *name, Fn fun)
    {
        return predicate<Tp>(name, std::function<bool(const Tp &)>(fun));
    } 
}

///////////////////////////////////////////////////////////////////////////////////////////////////////

#define Assert(...)            XPASTE(YATS_ASSERT_       ,PP_NARG(__VA_ARGS__)) ( __VA_ARGS__) 
#define AssertThrow(...)       XPASTE(YATS_ASSERT_THROW_ ,PP_NARG(__VA_ARGS__)) ( __VA_ARGS__) 
#define AssertNothrow(value)   YATS_ASSERT_NOTHROW(value, __LINE__)

#define StaticError(expr,msg)  XPASTE(YATS_STATIC_ERROR_, __COUNTER__) (expr,msg)

#define Context(ctx) \
namespace ctx { static const char _context_name[] = #ctx; } \
namespace ctx
 
#define Test(name) \
void test_ ## name(const char *); \
yats::task_register hook_ ## name(test_ ## name, task_register::type::test, _context_name, #name); \
void test_ ## name(const char *_name)

#define Setup(name) \
void setup_ ## name(const char *); \
yats::task_register fixture_ ## name(setup_ ## name, task_register::type::setup, _context_name); \
void setup_ ## name(const char *)

#define Teardown(name) \
void teardown_ ## name(const char *); \
yats::task_register fixture_ ## name(teardown_ ## name, task_register::type::teardown, _context_name); \
void teardown_ ## name(const char *)


#endif /* _YATS_HPP_ */
