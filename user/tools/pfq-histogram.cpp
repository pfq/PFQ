#include <affinity.hpp>

#include <iostream>
#include <fstream>
#include <sstream>

#include <thread>
#include <string>
#include <cstring>
#include <vector>
#include <algorithm>
#include <iterator>
#include <atomic>
#include <cmath>
#include <tuple>

#include <pfq.hpp>


using namespace net;

int
main(int argc, char *argv[])
try
{
    if (argc < 5)
       throw std::runtime_error(std::string("usage: ").append(argv[0]).append(" dev heap-size n-bin bin-size(ns)"));

    size_t heap_size = atoi(argv[2]);
    size_t nbin = atoi(argv[3]);
    size_t bin_size = atoi(argv[4]);

    // open a pfq socket:
    //

    pfq q(64);


    // add the device to this queue (hw queue = any)
    //
    q.bind(argv[1]);


    // select tstamp type:
    //
    q.timestamp_enable(true);

    // enable capturng for this queue:
    //
    q.enable();

    // create the heap (nanoseconds)
    //
    std::vector<int64_t> heap;
    heap.reserve(heap_size);


    std::vector<uint32_t> hist(nbin);

    // read packets:
    //

    uint64_t last = 0;

    std::make_heap(heap.begin(), heap.end(), std::less<uint64_t>());

    for(int j = 0; j < 1024;j++)
    {
        auto b = q.read(1000000);

        // std::cout << "batch size: " << b.size() << std::endl;

        std::for_each(b.begin(), b.end(), [&](pfq_pkt_hdr &h) {

           while(!net::data_ready(h, q.current_commit()))
           {
                std::this_thread::yield();
           }

           // this time stamp ...
           //
           uint64_t ts = static_cast<int64_t>(h.tstamp.tv.sec) * 1000000000 + h.tstamp.tv.nsec;

           heap.push_back(ts);
           std::push_heap(heap.begin(), heap.end(), std::greater<uint64_t>());

           // this is ok: wait until the heap is full!
           //
           if (heap.size() < heap_size)
                return;

           // get the next packet:
           uint64_t next = heap.front();

           if (next < last) {
                std::cout << "next: " << next  << " last:" << last << std::endl;
               throw std::runtime_error("negative index: heap must be too small");
           }

           size_t i = (next-last)/bin_size;
           if (i < nbin) {
                hist[i]++;
           }
           else {
                // std::cout << "bin: out of range: index = " << i << std::endl;
           }

           std::pop_heap(heap.begin(), heap.end(), std::greater<uint64_t>());
           heap.pop_back();

           //std::cout << "ts: " << h.tstamp.tv.sec << " " << h.tstamp.tv.nsec << " -> " << next << std::endl;

           last = next;
        });

        if( !( j & 15)) {
            std::copy(hist.begin(), hist.end(), std::ostream_iterator<uint64_t>(std::cout, " "));
            std::cout << std::endl << std::endl;
        }
    }

    return 0;
}
catch(std::exception &e)
{
    std::cerr << e.what() << std::endl;
}

