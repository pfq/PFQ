#include <pfq.hpp>

#include "yats.hpp"

using namespace yats;
using namespace net;

Context(PFQ)
{
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
        x.open(64);
        
        Assert(x.fd(), is_not_equal_to(-1));
        Assert(x.id(), is_not_equal_to(-1)); 
        AssertThrow( x.open(128) );

        x.close();
        Assert(x.fd(), is_equal_to(-1));
    }

    
    Test(enable_disable)
    {
        pfq x;
        
        AssertThrow(x.enable());
        AssertThrow(x.disable());

        x.open(64);
        
        x.enable();
        
        Assert(x.mem_addr());

        x.disable();

        Assert(x.mem_addr() == nullptr);
    }

    
    Test(is_enabled)
    {
        pfq x;
        Assert(x.is_enabled(), is_equal_to(false));
        x.open(64);
        Assert(x.is_enabled(), is_equal_to(false));
        x.enable();
        Assert(x.is_enabled(), is_equal_to(true));
    }


    Test(load_balance)
    {
        pfq x;
        AssertThrow(x.load_balance(true));
        x.open(64);
        x.load_balance(false);
    }

    
    Test(ifindex)
    {
        pfq x;
        AssertThrow(net::ifindex(1, "lo"));
        
        x.open(64);
        Assert( net::ifindex(x.fd(), "lo"), is_not_equal_to(-1));
    }


    Test(timestamp)
    {
        pfq x;
        AssertThrow(x.toggle_time_stamp(true));
        AssertThrow(x.time_stamp());

        x.open(64);
        x.toggle_time_stamp(true);

        Assert(x.time_stamp(), is_equal_to(true));
    }


    Test(caplen)
    {
        pfq x;
        AssertThrow(x.caplen(64));
        AssertThrow(x.caplen());

        x.open(64);
        x.caplen(128);

        Assert(x.caplen(), is_equal_to(128));
   
        x.enable();
        AssertThrow(x.caplen(64));
        x.disable();
        
        x.caplen(64);
        Assert(x.caplen(), is_equal_to(64));
    }
    
    
    Test(offset)
    {
        pfq x;
        AssertThrow(x.offset(14));
        AssertThrow(x.offset());

        x.open(64);
        x.offset(14);

        Assert(x.offset(), is_equal_to(14));
   
        x.enable();
        AssertThrow(x.offset(16));
        x.disable();
        
        x.offset(16);
        Assert(x.offset(), is_equal_to(16));
    }


    Test(slots)
    {
        pfq x;
        AssertThrow(x.slots(14));
        AssertThrow(x.slots());

        x.open(64);
        x.slots(1024);

        Assert(x.slots(), is_equal_to(1024));
   
        x.enable();
        AssertThrow(x.slots(4096));
        x.disable();
        
        x.slots(4096);
        Assert(x.slots(), is_equal_to(4096));
    }


    Test(slot_size)
    {
        pfq x;
        AssertThrow(x.slot_size());

        x.open(64);
        
        Assert(x.slot_size(), is_equal_to(80));
    }


    Test(add_device)
    {
        pfq x;
        AssertThrow(x.add_device("eth0"));

        x.open(64);
    
        AssertThrow(x.add_device("unknown"));
        x.add_device("eth0");
    }
    

    Test(remove_device)
    {
        pfq x;
        AssertThrow(x.remove_device("eth0"));

        x.open(64);
        
        AssertThrow(x.remove_device("unknown"));
        x.remove_device("eth0");
    }


    Test(poll)
    {
        pfq x;
        AssertThrow(x.poll(10));

        x.open(64);
        x.poll(0);
    }


    Test(read)
    {
        pfq x;
        AssertThrow(x.read(10));

        x.open(64);
        AssertThrow(x.read(10));
        
        x.enable();
        Assert(x.read(10).empty());
    }


    Test(stats)
    {
        pfq x;
        AssertThrow(x.stats());

        x.open(64);

        auto s = x.stats();
        Assert(s.recv, is_equal_to(0));
        Assert(s.lost, is_equal_to(0));
        Assert(s.drop, is_equal_to(0));
    }
}


int main()
{
    return yats::run();
}
