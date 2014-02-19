#include <stdexcept>

#include <pfq.hpp>
using namespace net;

/* Frame (98 bytes) */

static const unsigned char ping[98] =
{
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xf0, 0xbf, /* L`..UF.. */
    0x97, 0xe2, 0xff, 0xae, 0x08, 0x00, 0x45, 0x00, /* ......E. */
    0x00, 0x54, 0xb3, 0xf9, 0x40, 0x00, 0x40, 0x01, /* .T..@.@. */
    0xf5, 0x32, 0xc0, 0xa8, 0x00, 0x02, 0xad, 0xc2, /* .2...... */
    0x23, 0x10, 0x08, 0x00, 0xf2, 0xea, 0x42, 0x04, /* #.....B. */
    0x00, 0x01, 0xfe, 0xeb, 0xfc, 0x52, 0x00, 0x00, /* .....R.. */
    0x00, 0x00, 0x06, 0xfe, 0x02, 0x00, 0x00, 0x00, /* ........ */
    0x00, 0x00, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, /* ........ */
    0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, /* ........ */
    0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, /* .. !"#$% */
    0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, /* &'()*+,- */
    0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, /* ./012345 */
    0x36, 0x37                                      /* 67 */
};


void mode_0(pfq &q, int num)
{
    for(int n = 0; n < num; ++n)
    {
        q.inject(net::const_buffer(reinterpret_cast<const char *>(ping), sizeof(ping)));

        q.tx_queue_flush();
    }
}

void mode_1(pfq &q, int num)
{
    for(int n = 0; n < num; ++n)
    {
        q.inject(net::const_buffer(reinterpret_cast<const char *>(ping), sizeof(ping)));

        q.wakeup_tx_thread();
    }
}

void mode_2(pfq &q, int num)
{
    for(int n = 0; n < num; ++n)
    {
        q.send_async(net::const_buffer(reinterpret_cast<const char *>(ping), sizeof(ping)));
    }

    q.wakeup_tx_thread();
}


void mode_3(pfq &q, int num)
{
    for(int n = 0; n < num; ++n)
    {
        q.send(net::const_buffer(reinterpret_cast<const char *>(ping), sizeof(ping)));
    }
}


int
main(int argc, char *argv[])
{
    if (argc < 4)
        throw std::runtime_error(std::string("usage: ").append(argv[0]).append(" dev num mode"));

    const char *dev = argv[1];

    int num  = atoi(argv[2]);
    int mode = atoi(argv[3]);

    pfq q(128);

    q.enable();

    q.bind_tx(dev, -1);

    q.start_tx_thread(0);

    switch(mode)
    {
    case 0: mode_0(q, num); break;
    case 1: mode_1(q, num); break;
    case 2: mode_2(q, num); break;
    case 3: mode_3(q, num); break;
    default:
            throw std::runtime_error("unknown mode " + std::to_string(mode));
    }

    std::this_thread::sleep_for(std::chrono::seconds(1));
    q.stop_tx_thread();

    return 0;
}

