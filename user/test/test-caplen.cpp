#include <cstdio>
#include <string>
#include <stdexcept>

#include <pfq.hpp>
using namespace net;

int
main(int, char *[])
{
    pfq q(128);

    assert(q.caplen() == 128);
    
    q.enable();

    assert(q.caplen() == 128);

    try {
        q.caplen(64);
        assert(!"could set caplen while pfq is enabled!");
    }
    catch(...)
    { }

    q.disable();

    q.caplen(64);
    q.enable();

    assert(q.caplen() == 64);

    return 0;
}
 
