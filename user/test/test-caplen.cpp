#include <cstdio>
#include <string>
#include <stdexcept>

#include <pfq.hpp>
using namespace net;


int
main(int argc, char *argv[])
{
    pfq q(pfq_open);

    q.caplen(128);

    assert(q.caplen() == 128);

    return 0;
}
 
