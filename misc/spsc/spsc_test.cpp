#include "spsc_fifo.h"


#include <thread>
#include <iostream>


core_spsc_fifo *fifo;

unsigned long top = 1000000000UL;


void producer()
{
    for(auto n = 1ULL; n <= top;)
        if (core_spsc_push(fifo, (void *)n))
            n++;
}


void consumer()
{
    unsigned long sum = 0;

    for(auto n = 1ULL; n <= top;)
    {
        auto p = core_spsc_pop(fifo);
        if (p != NULL) {
            sum += (unsigned long)p;
            n++;
        }
    }

    std::cout << "total pop: " << sum << std::endl;
}


int
main(int argc, char *argv[])
{
    fifo = core_spsc_init(1000000);
    
    std::cout << "total sum: " << ((top*(top+1))/2) << std::endl;

    std::thread(producer).detach();
    std::thread(consumer).join();

    
    return 0;
}

