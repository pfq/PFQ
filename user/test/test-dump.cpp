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
    
    pfq r(1514);

    r.bind(argv[1], pfq::any_queue);

    r.toggle_time_stamp(true);
    
    r.enable();
    
    for(;;)
    {
            auto many = r.read( 1000000 /* timeout: micro */);

            queue::iterator it = many.begin();
            queue::iterator it_e = many.end();

            std::cout << "batch size: " << many.size() << " ===>" << std::endl;

            for(; it != it_e; ++it)
            {
                    while(!it->commit);

                    printf("caplen:%d len:%d ifindex:%d hw_queue:%d tstamp: %u:%u -> ", it->caplen, it->len, it->if_index, it->hw_queue,
                                                                                       it->tstamp.tv.sec, it->tstamp.tv.nsec);
                    char *buff = static_cast<char *>(it.data());

                    for(int x=0; x < std::min<int>(it->caplen, 34); x++)
                    {
                        printf("%2x ", (unsigned char)buff[x]);
                    }
                    printf("\n");
            }
    }

    return 0;
}
 
