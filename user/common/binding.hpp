/***************************************************************
 *
 * (C) 2014 - Nicola Bonelli <nicola@pfq.io>
 *
 ****************************************************************/

#pragma once

#include <vector>
#include <string>

namespace pfq {

    //
    // eth0:...:ethx[.core[.gid[.queue.queue...]]]
    //

    struct binding
    {
        std::vector<std::string>    dev;
        std::vector<int>            queue;
        int                         gid;
        int                         core;
    };


    static inline std::string
    show_binding(const binding &b)
    {
        std::string ret = "binding:{ ";
        int n = 0;

        ret += "dev:[";

        for(auto &d : b.dev)
        {
            if (n++)
                ret += ", ";
            ret += d;
        }
        ret += "] queue:[";

        n = 0;
        for(auto &q : b.queue)
        {
            if (n++)
                ret += ", ";
            ret += std::to_string(q);
        }
        ret += "] gid:" + std::to_string(b.gid) + " core:" + std::to_string (b.core);

        return ret + " }";
    }


    static inline
    binding
    make_binding(const char *value)
    {
        binding ret { {}, {}, -1, -1 };

        auto vec = pfq::split(value, ".");

        ret.dev = pfq::split(vec[0].c_str(), ":");

        if (vec.size() > 1)
            ret.core = std::atoi(vec[1].c_str());

        if (vec.size() > 2)
            ret.gid = std::atoi(vec[2].c_str());

        if (vec.size() > 3)
        {
            unsigned int n = 3;
            for(; n != vec.size(); n++)
            {
                ret.queue.push_back(std::atoi(vec[n].c_str()));
            }
        }

        return ret;
    }

} // namespace pfq
