#include <cstdio>
#include <string>
#include <stdexcept>

#include <pfq/pfq.hpp>

int
main(int argc, char *argv[])
{
    if (argc < 2)
        throw std::runtime_error(std::string("usage: ").append(argv[0]).append(" dev"));

    pfq::socket r(128);

    r.bind(argv[1], pfq::any_queue);

    r.timestamp_enable(true);

    struct sock_fprog p { 1, nullptr };

    r.set_group_fprog(r.group_id(), p);

    std::this_thread::sleep_for(std::chrono::seconds(1));

    r.reset_group_fprog(r.group_id());

    return 0;
}

