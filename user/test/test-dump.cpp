#include <iostream>
#include <cstdio>
#include <string>
#include <stdexcept>

#include <pfq/pfq.hpp>

int
main(int argc, char *argv[])
{
    if (argc < 2)
        throw std::runtime_error(std::string("usage: ").append(argv[0]).append(" dev"));

    pfq::socket q(128);

    q.bind(argv[1], pfq::any_queue);

    q.timestamping_enable(true);

    q.enable();

    for(;;)
    {
            auto many = q.read( 1000000 /* timeout: micro */);

            pfq::net_queue::iterator it = many.begin();
            pfq::net_queue::iterator it_e = many.end();

            for(; it != it_e; ++it)
            {
                    while (!it.ready()) {
                        std::this_thread::yield();
                    }

                    printf("vlan:%d caplen:%d len:%d ifindex:%d hw_queue:%d -> ",
                           it->vlan.vid, it->caplen, it->len, it->if_index, it->queue);

                    char *buff = static_cast<char *>(it.data());

                    for(int x=0; x < std::min<int>(it->caplen, 34); x++)
                    {
                        printf("%2x ", (unsigned char)buff[x]);
                    }
                    printf("\n");
            }

            auto cs = q.group_counters(q.group_id());

            std::cout << "counters: ";
            for(auto c : cs)
            {
                std::cout << c << ' ';
            }

            std::cout << std::endl;
    }
}

