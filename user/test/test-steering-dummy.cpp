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
    
    pfq r(128);

    r.bind(argv[1], pfq::any_queue);

    r.timestamp_enabled(true);
    
    r.set_group_function(r.group_id(), "steer-dummy");

    r.enable();
    
    for(int i = 0; i < 5;++i)
    {
            r.read( 1000000 /* timeout: micro */);
    }
    
    int state = 42;
    r.set_group_state(r.group_id(), state); 

    for(int i = 0; i < 5;++i)
    {
            r.read( 1000000 /* timeout: micro */);
    }

    r.reset_group_state(r.group_id()); 
    
    for(int i = 0; i < 5;++i)
    {
            r.read( 1000000 /* timeout: micro */);
    }

    return 0;
}
 
