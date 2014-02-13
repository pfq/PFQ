#include <stdexcept>

#include <pfq.hpp>
using namespace net;

char packet[] = "\x01\x02\x04\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x08\x00"
                "\x45\x02\x03\x06\x05\x06\x03\x08\x03\x0a\x01\x0c"
                "\x45\x02\x04\x04\x05\x06\x05\x08\x02\x0a\x03\x0c";

void mode_0(pfq &q)
{
    for(int n = 0; n < 1000; ++n)
    {
        q.inject(net::const_buffer(packet, sizeof(packet)));

        q.tx_queue_flush();
    }
}

void mode_1(pfq &q)
{
    for(int n = 0; n < 1000; ++n)
    {
        q.inject(net::const_buffer(packet, sizeof(packet)));

        q.wakeup_tx_thread();
    }
}

void mode_2(pfq &q)
{
    for(int n = 0; n < 1000; ++n)
    {
        q.send_async(net::const_buffer(packet, sizeof(packet)));
    }

    q.wakeup_tx_thread();
}


void mode_3(pfq &q)
{
    for(int n = 0; n < 1000; ++n)
    {
        q.send(net::const_buffer(packet, sizeof(packet)));
    }
}


int
main(int argc, char *argv[])
{
    if (argc < 3)
        throw std::runtime_error(std::string("usage: ").append(argv[0]).append(" dev int"));

    const char *dev = argv[1];

    int mode = atoi(argv[2]);

    pfq q(128);

    q.enable();

    q.bind_tx(dev, -1);

    q.start_tx_thread(0);

    switch(mode)
    {
    case 0: mode_0(q); break;
    case 1: mode_1(q); break;
    case 2: mode_2(q); break;
    case 3: mode_3(q); break;
    default:
            throw std::runtime_error("unknown mode " + std::to_string(mode));
    }

    std::this_thread::sleep_for(std::chrono::seconds(1));
    q.stop_tx_thread();

    return 0;
}

