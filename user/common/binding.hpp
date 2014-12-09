/***************************************************************
 *
 * (C) 2014 - Nicola Bonelli <nicola@pfq.io>
 *
 ****************************************************************/

#pragma once

#include <vector>
#include <string>

#include <pfq/util.hpp>


namespace pfq {

    constexpr const char binding_format[] = "gid[.core[.eth0:eth1:eth2...[.queue.queue.queue...]]]";

    struct binding
    {
        int                         gid;
        int                         core;
        std::vector<std::string>    dev;
        std::vector<int>            queue;
    };


    static inline std::string
    show_binding(const binding &b)
    {
        std::string ret = "binding:{ ";
        int n = 0;

        ret += "gid:" + std::to_string(b.gid) +
               " core:" + std::to_string(b.core) +
               " dev:[";

        for(auto &dev : b.dev)
        {
            if (n++)
                ret += ", ";
            ret += dev;
        }

        ret += "] queue:[";

        n = 0;
        for(auto &q : b.queue)
        {
            if (n++)
                ret += ", ";
            ret += std::to_string(q);
        }

        return ret + "] }";
    }


    static inline
    binding
    make_binding(const char *value)
    {
        binding ret { 0, 0, {}, {} };

        auto vec = pfq::split(value, ".");

        ret.gid = std::atoi(vec.at(0).c_str());

        if (vec.size() > 1)
            ret.core = std::atoi(vec[1].c_str());

        if (vec.size() > 2)
            ret.dev = pfq::split(vec[2].c_str(), ":");

        if (vec.size() > 3)
        {
            for(size_t n = 3; n < vec.size(); n++)
            {
                ret.queue.push_back(std::atoi(vec[n].c_str()));
            }
        }

        return ret;
    }

} // namespace pfq
