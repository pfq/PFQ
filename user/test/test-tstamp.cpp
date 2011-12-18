#include <cstdio>
#include <string>
#include <stdexcept>

#include <pfq.hpp>
using namespace net;

int
main(int argc, char *argv[])
{
    if (argc < 2)
        throw std::runtime_error(std::string("usage: ").append(argv[0]).append(" dev"));
    
    pfq q(pfq_open);

    q.add_device(argv[1], pfq::any_queue);

    q.tstamp(true);
    
    sleep(1);

    q.tstamp(true);

    sleep(1);
    
    q.tstamp(false);
    
    sleep(1);

    return 0;
}
 
