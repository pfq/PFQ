#include <iostream>
#include <cstdio>
#include <string>
#include <stdexcept>

#include <pfq.hpp>

using namespace pfq;

int
main(int argc, char *argv[])
{
    if (argc < 2)
        throw std::runtime_error(std::string("usage: ").append(argv[0]).append(" dev"));

    pfq::socket r(1514);

    r.bind(argv[1], pfq::any_queue);

    r.timestamp_enable(true);

    r.enable();

    for(;;)
    {
            auto many = r.read( 1000000 /* timeout: micro */);

            std::cout << "batch size: " << many.size() << " ===>" << std::endl;

#if __GNUC__ == 4 &&  __GNUC_MINOR__ >= 6

            for(auto & packet : many)
            {
                char *buff;
                while(!(buff = static_cast<char *>(data_ready(packet, many.index()))))
                    std::this_thread::yield();

                for(int x=0; x < std::min<int>(packet.caplen, 34); x++)
                {
                    printf("%2x ", (unsigned char)buff[x]);
                }
                printf("\n");
            }
#endif

    }
}

