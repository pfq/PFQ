/***************************************************************
 *
 * (C) 2014 - Nicola Bonelli <nicola@pfq.io>
 *
 ****************************************************************/

#pragma once

namespace more { namespace vt100
{
    namespace
    {
        const char * const CLEAR = "\E[2J";
        const char * const EDOWN = "\E[J";
        const char * const DOWN  = "\E[1B";
        const char * const HOME  = "\E[H";
        const char * const ELINE = "\E[K";
        const char * const BOLD  = "\E[1m";
        const char * const RESET = "\E[0m";
        const char * const BLUE  = "\E[1;34m";
        const char * const RED   = "\E[31m";
    }
}}

