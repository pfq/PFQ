#include <cstdio>
#include <string>
#include <stdexcept>
#include <cassert>

#include <pfq.hpp>
using namespace net;

int
main(int, char *[])
{
    pfq q;

    q.caplen(64);
    assert(q.caplen() == 64);

    q.slots(1024);
    assert(q.slots() == 1024);
    
    std::cout << "caplen   :" << q.caplen()    << std::endl;
    std::cout << "slot size:" << q.slot_size() << std::endl;
    assert(q.slot_size() == 80);
    
    q.caplen(1500);
    std::cout << "caplen   :" << q.caplen()    << std::endl;
    std::cout << "slot size:" << q.slot_size() << std::endl;
    assert(q.slot_size() == 1520);
    
    return 0;
}
 
