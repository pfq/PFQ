#include <cstdio>
#include <string>
#include <stdexcept>

#include <pfq.hpp>
using namespace net;

int
main(int, char *[])
{
    pfq q(128, 14);

    assert(q.offset() == 14);
    
    q.offset(0);

    assert(q.offset() == 0);

    return 0;
}
 
