#include <pfq.hpp>
#include <cstdio>

using namespace net;

int
main(int argc, char *argv[])
{
    pfq r(pfq_open);

    r.add_device(r.ifindex("eth3"), pfq::any_queue);

    r.enable();

    r.tstamp(true);

    // assert(r.num_slots() == 4096);

    int i = 0;

    char * buffer = new char[r.queue_size()];

    for(;; i++)
    {
            batch many = r.read( 1000000 /* timeout: micro */);

            batch::iterator it = many.begin();
            batch::iterator it_e = many.end();

            std::cout << "batch size: " << many.size() << std::endl;

            printf("-----------------------\n");

            for(; it != it_e; ++it)
            {
                    while(!it->commit);

                    printf("caplen:%d len:%d ifindex:%d hw_queue:%d tstamp: %u:%u -> ", it->caplen, it->len, it->if_index, it->hw_queue,
                                                                                        it->tstamp.tv.sec, it->tstamp.tv.nsec);
                    char *buff = it.data();

                    for(int x=0; x < std::min<int>(it->caplen, 34); x++)
                    {
                        printf("%2x ", (unsigned char)buff[x]);
                    }
                    printf("\n");
            }

            // usleep(200000);
    }

    delete []buffer;

    return 0;
}
 
