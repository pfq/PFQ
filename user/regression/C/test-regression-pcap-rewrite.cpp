#include <net/ethernet.h>
#include <arpa/inet.h>
#include <linux/ip.h>

#include <pcap.h>

#include <cstring>
#include <stdexcept>
#include <iostream>

namespace
{
    pcap_t * in;
    pcap_dumper_t * out;
    unsigned int counter = 0;
}

static inline
void handler(u_char *, const struct pcap_pkthdr *h, const u_char *bytes)
{
    u_char packet[2000];

    if (h->caplen < 34)
       return;

    auto e = reinterpret_cast<const ether_header *>(bytes);
    if (e->ether_type != htons(ETHERTYPE_IP)) {
        std::cout << "skip: ether_type = " << ntohs(e->ether_type) << std::endl;
        return;
    }

    memcpy(packet, bytes, h->caplen);

    auto ip = reinterpret_cast<iphdr *>(packet + sizeof(ether_header));

    ip->saddr = counter++;

    pcap_dump(reinterpret_cast<u_char *>(out), h, packet);
}


int
main(int argc, char *argv[])
{
    char errbuf[PCAP_ERRBUF_SIZE];

    if (argc < 3) {
        std::cerr << "usage: in.pcap out.pcap" << std::endl;
        return -1;
    }

    char *file_in = argv[1], *file_out = argv[2];

    std::cout << "pfq_test: opening " << file_in << " for reading..." << std::endl;
    in = pcap_open_offline(file_in, errbuf);
    if (in == nullptr)
        throw std::runtime_error(errbuf);

    out = pcap_dump_open(in, file_out);
    if (out == nullptr)
        throw std::runtime_error(errbuf);

    std::cout << "rewriting packets to " << file_out << ':' << std::endl;

    if (pcap_loop(in, 0, handler, 0) < 0)
        throw std::runtime_error(pcap_geterr(in));

    std::cout << counter << " IP packets written." << std::endl;
    return 0;
}


