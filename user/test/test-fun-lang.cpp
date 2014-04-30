#include <iostream>
#include <string>
#include <stdexcept>
#include <chrono>

#include <pfq.hpp>
#include <pfq-lang.hpp>
#include <pfq-default.hpp>

using namespace net;
using namespace pfq_lang;

int
main(int argc, char *argv[])
{
    if (argc < 2)
        throw std::runtime_error(std::string("usage: ").append(argv[0]).append(" dev"));

    pfq q(128);

    q.bind(argv[1], pfq::any_queue);

    auto gid = q.group_id();

    // ip >-> counter 0
    // >-> conditional is_icmp
    //     (counter 1 >-> mark 1 >-> steer_ip >-> when' (has_mark 1) (counter 2))
    //     drop'

    auto comp = ip >> addr("192.168.0.0", 16) >> unit >> counter (0) >>
                    conditional (is_icmp & has_addr("192.168.0.0", 16) & any_bit(ip_id, 0xffffff),
                                 (counter (1) >> mark (1) >> steer_ip >> when (has_mark (1), counter (2))),
                                  drop);

    q.set_group_computation(gid, comp);

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

                    auto cs = q.group_counters(gid);

                    printf("counters: { ");
                    for(auto c : cs)
                        printf("%lu ", c);

                    printf("}\n");
            }
    }

    return 0;
}

