#include <cstdio>
#include <string>
#include <stdexcept>

#include <pfq.hpp>
using namespace net;

int
main(int argc, char *argv[])
{
    pfq q(pfq_open);

    q.tstamp(true);
    
    sleep(1);

    q.tstamp(true);

    sleep(1);
    
    q.tstamp(false);
    
    sleep(1);

    return 0;
}
 
