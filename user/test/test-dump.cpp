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

    pfq q(128);

    q.bind(argv[1], pfq::any_queue);

    q.timestamp_enable(true);

    q.enable();

    for(;;)
    {
            auto many = q.read( 1000000 /* timeout: micro */);

            queue::iterator it = many.begin();
            queue::iterator it_e = many.end();

            for(; it != it_e; ++it)
            {
                    while (!it.ready()) {
                        std::this_thread::yield();
                    }

                    printf("vlan:%d caplen:%d len:%d ifindex:%d hw_queue:%d -> ",
                           it->un.vlan.vlan_vid, it->caplen, it->len, it->if_index, it->hw_queue);

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

