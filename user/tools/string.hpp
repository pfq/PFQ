/***************************************************************
 *
 * (C) 2014 - Nicola Bonelli <nicola.bonelli@cnit.it>
 *
 ****************************************************************/

#pragma once

#include <iostream>
#include <string>
#include <vector>
#include <cstring>


inline std::string
trim(std::string str)
{
    auto b = str.find_first_not_of(" \n\r\t");
    auto e = str.find_last_not_of(" \n\r\t");
    b = b == std::string::npos ? 0 : b;
    e = e == std::string::npos ? std::string::npos : (e + 1 - b);
    return str.substr(b, e);
}


inline std::vector<std::string>
split(std::string str, const char *sep)
{
    std::vector<std::string> ret;
    std::string::size_type n;

    auto len = std::strlen(sep);

    for(std::string::size_type n; (n = str.find(sep)) != std::string::npos;)
    {
        ret.emplace_back(str.substr(0,n));
        str = str.substr(n + len, std::string::npos);
    }

    if (!str.empty())
        ret.push_back(std::move(str));

    return ret;
}
