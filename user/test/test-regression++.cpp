#include <pfq.hpp>
#include <future>
#include <system_error>

#include <sys/types.h>
#include <sys/wait.h>

#include "yats.hpp"

using namespace yats;
using namespace net;

Context(PFQ)
{
    const std::string DEV("eth0");

    Test(default_ctor_dtor)
    {
        pfq x;
        Assert(x.id(), is_equal_to(-1));
    }

    Test(move_ctor)
    {
        pfq x(64);
        pfq y(std::move(x));

        Assert(x.fd(), is_equal_to(-1));
        Assert(y.fd(), is_not_equal_to(-1));
    }


    Test(assign_move_oper)
    {
        pfq x(64);
        pfq y;
        y = std::move(x);

        Assert(x.fd(), is_equal_to(-1));
        Assert(y.fd(), is_not_equal_to(-1));
    }


    Test(swap)
    {
        pfq x(64);
        pfq y;
        x.swap(y);

        Assert(x.fd(), is_equal_to(-1));
        Assert(y.fd(), is_not_equal_to(-1));
    }


    Test(open_close)
    {
        pfq x;
        x.open(group_policy::undefined, 64);

        Assert(x.fd(), is_not_equal_to(-1));
        Assert(x.id(), is_not_equal_to(-1));
        AssertThrow( x.open(group_policy::undefined, 128) );

        x.close();
        Assert(x.fd(), is_equal_to(-1));
    }


    Test(enable_disable)
    {
        pfq x;

        AssertThrow(x.enable());
        AssertThrow(x.disable());

        x.open(group_policy::undefined, 64);

        x.enable();

        Assert(x.mem_addr());

        x.disable();

        Assert(x.mem_addr() == nullptr);
    }


    Test(enabled)
    {
        pfq x;
        Assert(x.enabled(), is_equal_to(false));
        x.open(group_policy::undefined, 64);
        Assert(x.enabled(), is_equal_to(false));
        x.enable();
        Assert(x.enabled(), is_equal_to(true));
    }


    Test(ifindex)
    {
        pfq x;
        AssertThrow(net::ifindex(1, "lo"));

        x.open(group_policy::undefined, 64);
        Assert( net::ifindex(x.fd(), "lo"), is_not_equal_to(-1));
    }


    Test(timestamp)
    {
        pfq x;
        AssertThrow(x.timestamp_enable(true));
        AssertThrow(x.timestamp_enabled());

        x.open(group_policy::undefined, 64);
        x.timestamp_enable(true);

        Assert(x.timestamp_enabled(), is_equal_to(true));
    }


    Test(caplen)
    {
        pfq x;
        AssertThrow(x.caplen(64));
        AssertThrow(x.caplen());

        x.open(group_policy::undefined, 64);
        x.caplen(128);

        Assert(x.caplen(), is_equal_to(128));

        x.enable();
        AssertThrow(x.caplen(64));
        x.disable();

        x.caplen(64);
        Assert(x.caplen(), is_equal_to(64));
    }


    Test(maxlen)
    {
        pfq x;
        AssertThrow(x.maxlen(64));
        AssertThrow(x.maxlen());

        x.open(group_policy::undefined, 64);
        x.maxlen(128);

        Assert(x.maxlen(), is_equal_to(128));

        x.enable();
        AssertThrow(x.maxlen(64));
        x.disable();

        x.maxlen(64);
        Assert(x.maxlen(), is_equal_to(64));
    }

    Test(offset)
    {
        pfq x;
        AssertThrow(x.offset(14));
        AssertThrow(x.offset());

        x.open(group_policy::undefined, 64);
        x.offset(14);

        Assert(x.offset(), is_equal_to(14));

        x.enable();
        AssertThrow(x.offset(16));
        x.disable();

        x.offset(16);
        Assert(x.offset(), is_equal_to(16));
    }


    Test(rx_slots)
    {
        pfq x;
        AssertThrow(x.rx_slots(14));
        AssertThrow(x.rx_slots());

        x.open(group_policy::undefined, 64);
        x.rx_slots(1024);

        Assert(x.rx_slots(), is_equal_to(1024));

        x.enable();
        AssertThrow(x.rx_slots(4096));
        x.disable();

        x.rx_slots(4096);
        Assert(x.rx_slots(), is_equal_to(4096));
    }


    Test(rx_slot_size)
    {
        pfq x;
        AssertThrow(x.rx_slot_size());

        x.open(group_policy::undefined, 64);

        auto size = 64 + sizeof(pfq_pkt_hdr);
        Assert(x.rx_slot_size(), is_equal_to( size + (size % 8) ));
    }

    Test(tx_slots)
    {
        pfq x;
        AssertThrow(x.tx_slots(14));
        AssertThrow(x.tx_slots());

        x.open(group_policy::undefined, 64);
        x.tx_slots(1024);

        Assert(x.tx_slots(), is_equal_to(1024));

        x.enable();
        AssertThrow(x.tx_slots(4096));
        x.disable();

        x.tx_slots(4096);
        Assert(x.tx_slots(), is_equal_to(4096));
    }

    Test(bind_device)
    {
        pfq x;
        AssertThrow(x.bind(DEV.c_str()));

        x.open(group_policy::shared, 64);

        AssertThrow(x.bind("unknown"));
        x.bind(DEV.c_str());

        AssertThrow(x.bind_group(11, DEV.c_str()));
    }


    Test(unbind_device)
    {
        pfq x;
        AssertThrow(x.unbind(DEV.c_str()));

        x.open(group_policy::shared, 64);

        AssertThrow(x.unbind("unknown"));
        x.unbind(DEV.c_str());

        AssertThrow(x.unbind_group(11, DEV.c_str()));
    }


    Test(poll)
    {
        pfq x;
        AssertThrow(x.poll(10));

        x.open(group_policy::undefined, 64);
        x.poll(0);
    }


    Test(read)
    {
        pfq x;
        AssertThrow(x.read(10));

        x.open(group_policy::undefined, 64);
        AssertThrow(x.read(10));

        x.enable();
        Assert(x.read(10).empty());
    }


    Test(stats)
    {
        pfq x;
        AssertThrow(x.stats());

        x.open(group_policy::undefined, 64);

        auto s = x.stats();
        Assert(s.recv, is_equal_to(0));
        Assert(s.lost, is_equal_to(0));
        Assert(s.drop, is_equal_to(0));
    }

    Test(group_stats)
    {
        pfq x;

        x.open(group_policy::undefined, 64);

        AssertNothrow(x.group_stats(11));

        x.join_group(11);

        auto s = x.group_stats(11);
        Assert(s.recv, is_equal_to(0));
        Assert(s.lost, is_equal_to(0));
        Assert(s.drop, is_equal_to(0));
    }


    Test(my_group_stats_priv)
    {
        pfq x;

        x.open(group_policy::priv, 64);

        auto gid = x.group_id();

        AssertNothrow(x.group_stats(gid));

        auto s = x.group_stats(gid);
        Assert(s.recv, is_equal_to(0));
        Assert(s.lost, is_equal_to(0));
        Assert(s.drop, is_equal_to(0));
    }

    Test(my_group_stats_restricted)
    {
        pfq x;

        x.open(group_policy::restricted, 64);

        auto gid = x.group_id();

        AssertNothrow(x.group_stats(gid));

        auto s = x.group_stats(gid);
        Assert(s.recv, is_equal_to(0));
        Assert(s.lost, is_equal_to(0));
        Assert(s.drop, is_equal_to(0));
    }

    Test(my_group_stats_shared)
    {
        pfq x;

        x.open(group_policy::shared, 64);

        auto gid = x.group_id();

        AssertNothrow(x.group_stats(gid));

        auto s = x.group_stats(gid);
        Assert(s.recv, is_equal_to(0));
        Assert(s.lost, is_equal_to(0));
        Assert(s.drop, is_equal_to(0));
    }

    Test(groups_mask)
    {
        pfq x;
        AssertThrow(x.groups_mask());

        x.open(group_policy::undefined, 64);

        Assert(x.groups_mask(), is_equal_to(0));

        auto v = x.groups();
        Assert(v.empty(), is_true());
    }

    Test(join_restricted)
    {
        pfq x(group_policy::restricted, 64);

        pfq y;

        y.open(group_policy::undefined, 64);

        Assert( y.join_group(x.group_id(), group_policy::restricted), is_equal_to(x.group_id()));
    }

    Test(join_deferred)
    {
        pfq x(group_policy::undefined, 64);

        x.join_group(22);
        x.join_group(22);

        auto task = std::async(std::launch::async,
                    [&] {
                        pfq y(group_policy::undefined, 64);
                        Assert(y.join_group(22), is_equal_to(22));
                    });

        task.wait();
    }


    Test(join_restricted_thread)
    {
        pfq x(group_policy::restricted, 64);

        auto task = std::async(std::launch::async,
                    [&] {
                        pfq y(group_policy::undefined, 64);
                        Assert(y.join_group(x.group_id(), group_policy::restricted), is_equal_to(x.group_id()));
                    });

        task.get(); // eventually rethrow the excpetion...
    }

    Test(join_restricted_process)
    {
        pfq x(group_policy::restricted, 64);
        pfq z(group_policy::shared, 64);

        auto p = fork();
        if (p == -1)
            throw std::system_error(errno, std::generic_category());

        if (p == 0) {
            pfq y(group_policy::undefined, 64);;

            Assert( y.join_group(z.group_id()), is_equal_to(z.group_id()));
            AssertThrow(y.join_group(x.group_id()));

            _Exit(1);
        }

        wait(nullptr);
    }

    Test(join_private_)
    {
        pfq x(64);

        pfq y(group_policy::undefined, 64);

        AssertThrow(y.join_group(x.group_id(), group_policy::restricted));
        AssertThrow(y.join_group(x.group_id(), group_policy::shared));
        AssertThrow(y.join_group(x.group_id(), group_policy::priv));
        AssertThrow(y.join_group(x.group_id(), group_policy::undefined));
    }

    Test(join_restricted_)
    {
        {
            pfq x(group_policy::restricted, 64);
            pfq y(group_policy::undefined, 64);
            AssertNothrow(y.join_group(x.group_id(), group_policy::restricted));
        }
        {
            pfq x(group_policy::restricted, 64);
            pfq y(group_policy::undefined, 64);
            AssertThrow(y.join_group(x.group_id(), group_policy::shared));
        }
        {
            pfq x(group_policy::restricted, 64);
            pfq y(group_policy::undefined, 64);
            AssertThrow(y.join_group(x.group_id(), group_policy::priv));
        }
        {
            pfq x(group_policy::restricted, 64);
            pfq y(group_policy::undefined, 64);
            AssertThrow(y.join_group(x.group_id(), group_policy::undefined));
        }
    }

    Test(join_shared_)
    {
        {
            pfq x(group_policy::shared, 64);
            pfq y(group_policy::undefined, 64);
            AssertThrow(y.join_group(x.group_id(), group_policy::restricted));
        }
        {
            pfq x(group_policy::shared, 64);
            pfq y(group_policy::undefined, 64);
            AssertNothrow(y.join_group(x.group_id(), group_policy::shared));
        }
        {
            pfq x(group_policy::shared, 64);
            pfq y(group_policy::undefined, 64);
            AssertThrow(y.join_group(x.group_id(), group_policy::priv));
        }
        {
            pfq x(group_policy::shared, 64);
            pfq y(group_policy::undefined, 64);
            AssertThrow(y.join_group(x.group_id(), group_policy::undefined));
        }
    }


    Test(join_public)
    {
        pfq x;
        AssertThrow(x.join_group(12));

        x.open(group_policy::undefined, 64);
        int gid = x.join_group(0);
        Assert(gid, is_equal_to(0));

        gid = x.join_group(pfq::any_group);
        Assert(gid, is_equal_to(1));

        auto v = x.groups();

        Assert( v == (std::vector<int>{ 0, 1}) );

    }

    Test(leave_group)
    {
        pfq x;
        AssertThrow(x.leave_group(12));

        x.open(group_policy::shared, 64);
        int gid = x.join_group(22);
        Assert(gid, is_equal_to(22));

        x.leave_group(22);

        Assert(x.group_id(), is_equal_to(0));
        Assert(x.groups() == std::vector<int>{ 0 });
    }


    Test(gid)
    {
        pfq x;
        Assert(x.group_id(), is_equal_to(-1));
    }


    Test(vlan_enable)
    {
        pfq x(64);
        AssertNothrow(x.vlan_filters_enable(x.group_id(), true));
        AssertNothrow(x.vlan_filters_enable(x.group_id(), false));
    }

    Test(vlan_filt)
    {
        pfq x(64);
        AssertThrow(x.vlan_set_filter(x.group_id(), 22));
        AssertThrow(x.vlan_reset_filter(x.group_id(), 22));

        AssertNothrow(x.vlan_filters_enable(x.group_id(), true));
        AssertNothrow(x.vlan_set_filter(x.group_id(), 22));
        AssertNothrow(x.vlan_reset_filter(x.group_id(), 22));

        AssertNothrow(x.vlan_filters_enable(x.group_id(), false));
    }


    Test(bind_tx)
    {
        pfq q(64);
        AssertNothrow(q.bind_tx("lo", -1));
        AssertThrow(q.bind_tx("unknown", -1));
    }


    Test(start_tx_thread)
    {
        pfq q(64);
        AssertThrow(q.start_tx_thread(0));

        q.bind_tx("lo", -1);

        q.enable();

        AssertNothrow(q.start_tx_thread(0));
    }


    Test(stop_tx_thread)
    {
        pfq q(64);
        AssertThrow(q.stop_tx_thread());

        q.bind_tx("lo", -1);

        q.enable();
        q.start_tx_thread(0);

        AssertNothrow(q.stop_tx_thread());
    }

    Test(wakeup_tx_thread)
    {
        pfq q(64);
        AssertThrow(q.wakeup_tx_thread());

        q.bind_tx("lo", -1);

        q.enable();

        q.start_tx_thread(0);

        AssertNothrow(q.wakeup_tx_thread());
    }

    Test(tx_queue_flush)
    {
        pfq q(64);
        AssertThrow(q.tx_queue_flush());

        q.bind_tx("lo", -1);

        q.enable();

        AssertNothrow(q.tx_queue_flush());
    }


#if 0
    Test(group_context)
    {
        pfq x(group_policy::shared, 64);

        int n = 22;
        x.set_group_function_context(x.group_id(), n);

        int m = 0;
        x.get_group_function_context(x.group_id(), m);

        Assert(n, is_equal_to(m));

        x.bind(DEV.c_str());

        x.set_group_function(x.group_id(), "dummy-context", 0);
        x.set_group_function(x.group_id(), "clone",      1);

        x.enable();

        std::cout << "waiting for packets from " << DEV << "..." << std::flush;
        for(;;)
        {
            auto q = x.read(100000);
            if (q.empty())
            {
                std::cout << "." << std::flush;
                continue;
            }

            for(auto &hdr : q)
            {
                Assert(hdr.data, is_equal_to(22));
                Assert(hdr.gid,  is_equal_to(x.group_id()));
            }
            break;
        }
        std::cout << std::endl;
    }

#endif

}


int main(int argc, char *argv[])
{
    return yats::run(argc, argv);
}
