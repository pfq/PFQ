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

    q.timestamp_enable(true);

    q.enable();

    q.vlan_filters_enable(q.group_id(), true);
    q.vlan_set_filter(q.group_id(), pfq::vlan_id::anytag);

    for(;;)
    {
            auto many = q.read( 1000000 /* timeout: micro */);

            pfq::queue::iterator it = many.begin();
            pfq::queue::iterator it_e = many.end();

            std::cout << "batch size: " << many.size() << " ===>" << std::endl;

            for(; it != it_e; ++it)
            {
                    while(!it.ready());

                    printf("vlan_vid:%d vlan_prio:%d caplen:%d len:%d ifindex:%d hw_queue:%d tstamp: %u:%u -> ",
                           it->un.vlan.vlan_vid, it->un.vlan.vlan_prio,
                           it->caplen, it->len, it->if_index, it->hw_queue,
                                it->tstamp.tv.sec, it->tstamp.tv.nsec);
                    char *buff = static_cast<char *>(it.data());

                    for(int x=0; x < std::min<int>(it->caplen, 18); x++)
                    {
                        printf("%2x ", (unsigned char)buff[x]);
                    }
                    printf("\n");
            }
    }
}

