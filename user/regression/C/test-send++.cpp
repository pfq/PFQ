#include <iostream>
#include <stdexcept>

#include <pfq/pfq.hpp>

using namespace pfq;

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


void send_packets(pfq::socket &q, int64_t num)
{
    std::cout << "sending " << num << " packets:" << std::endl;

    for(int64_t n = 0; n < num;)
    {
        if (q.send(pfq::const_buffer(reinterpret_cast<const char *>(ping), sizeof(ping))))
            n++;
    }
}


void send_packets_async(pfq::socket &q, int64_t num)
{
    std::cout << "sending " << num << " packets (async):" << std::endl;
    for(int64_t n = 0; n < num;)
    {
        if (q.send_async(pfq::const_buffer(reinterpret_cast<const char *>(ping), sizeof(ping))))
            n++;
    }
}


int
main(int argc, char *argv[])
try
{
    if (argc < 5)
        throw std::runtime_error(std::string("usage: ").append(argv[0]).append(" dev queue kthread num"));

    const char *dev = argv[1];
    int queue       = atoi(argv[2]);
    int kthread     = atoi(argv[3]);
    int64_t num     = atoll(argv[4]);

    pfq::socket q(64, 1024, 1024);

    q.bind_tx(dev, queue, kthread);

    q.enable();

    if (kthread == -1) {
        send_packets(q, num);
    }
    else {
        send_packets_async(q, num);
    }

    std::this_thread::sleep_for(std::chrono::seconds(1));

    auto stat = q.stats();

    std::cout << "sent: " << stat.sent << " - disc: " << stat.disc << std::endl;

    return 0;
}
catch(std::exception &e)
{
    std::cout << e.what() << std::endl;
}

