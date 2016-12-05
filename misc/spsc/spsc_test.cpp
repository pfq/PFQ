#include "spsc_fifo_new.h"

#include <pthread.h>

#include <thread>
#include <iostream>


core_spsc_fifo *fifo;

unsigned long top = 1000000000UL;


inline
void set_affinity(std::thread &t, size_t n)
{
    if(t.get_id() == std::thread::id())
        throw std::runtime_error("thread not running");

    cpu_set_t cpuset;

    CPU_ZERO(&cpuset);
    CPU_SET(n, &cpuset);

    auto pth = t.native_handle();
    if ( ::pthread_setaffinity_np(pth, sizeof(cpuset), &cpuset) != 0)
        throw std::runtime_error("pthread_setaffinity_np");
}


void producer()
{
    for(auto n = 1ULL; n <= top;) {
        if (core_spsc_push(fifo, (void *)n)) {
            n++;
        }
    }

    core_spsc_push_sync(fifo);
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
    fifo = core_spsc_init(8192, 0);
    
    std::cout << "total sum: " << ((top*(top+1))/2) << std::endl;

    auto t = std::thread(producer);
    set_affinity(t, 0);
    t.detach();

    std::thread(consumer).join();
    
    return 0;
}

