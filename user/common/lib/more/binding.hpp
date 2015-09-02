/***************************************************************
 *
 * (C) 2014 - Nicola Bonelli <nicola@pfq.io>
 *
 ****************************************************************/

#pragma once

#include <vector>
#include <string>

#include <pfq/util.hpp>


namespace more {

    constexpr const char netdev_format[] = "DEVICE = NAME[:queue[,queue[,queue...]]]";

    struct netdev
    {
        std::string         name;
        std::vector<int>    queue;
    };

    static inline std::string
    show(const netdev &b)
    {
        std::string ret = "dev{" + b.name;

        if (!b.queue.empty())
        {
            ret += " [";
            int n = 0;
            for(auto &q : b.queue)
            {
                if (n++)
                    ret += " ";
                ret += std::to_string(q);
            }
            ret += ']';
        }

        return ret + '}';
    }

    static inline
    netdev read_device(const char *value)
    {
        netdev b;

        auto vec = pfq::split(value, ":");
        b.name = vec.at(0);
        if (vec.size() > 1) {
            auto qs = pfq::split(vec[1], ",");
            b.queue = pfq::fmap([](const std::string &q) {
                                        return std::stoi(q);
                                    }, std::move(qs));
        }

        return b;
    }

    constexpr const char thread_binding_format[] = "BINDING = cpu.gid[.DEVICE[.DEVICE[.DEVICE...]]]";

    struct thread_binding
    {
        int                         cpu;        // cpu id for thread affinity
        int                         gid;        // pfq gid
        std::vector<netdev>         dev;        // devs and related queues
    };

    static inline std::string
    show(const thread_binding &t)
    {
        std::string ret =
            "thread_binding{cpu:" + std::to_string(t.cpu) + " gid:" + std::to_string(t.gid);

        if (!t.dev.empty())
        {
            for(auto & b : t.dev)
                ret += ' ' + show(b);
        }

        return ret + '}';
    }


    static inline
    thread_binding
    read_thread_binding(const char *value)
    {
        auto vec = pfq::split(value, ".");
        if (vec.size() < 2)
            throw std::runtime_error("read_thread_binding: parse error");

        thread_binding t { std::stoi(vec[0]), std::stoi(vec[1]), { }};

        for(size_t n = 2; n < vec.size(); n++)
            t.dev.push_back(read_device(vec[n].c_str()));

        return t;
    }

} // namespace more
