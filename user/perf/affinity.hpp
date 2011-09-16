#ifndef _AFFINITY_HPP_
#define _AFFINITY_HPP_ 

#include <thread>
#include <pthread.h> // pthread_setaffinity_np

#include <stdexcept>

namespace extra { 

    static inline void 
    set_affinity(std::thread &t, int n) 
    {
        if(t.get_id() == std::thread::id())
            throw std::runtime_error("thread not running");

        cpu_set_t cpuset;
        CPU_ZERO(&cpuset); CPU_SET(n, &cpuset);

        auto pth = t.native_handle();
        if ( ::pthread_setaffinity_np(pth, sizeof(cpuset), &cpuset) != 0)
            throw std::runtime_error("pthread_setaffinity_np");
    }

} // namespace extra

#endif /* _AFFINITY_HPP_ */
