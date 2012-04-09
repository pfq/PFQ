/***************************************************************
   
   Copyright (c) 2012, Nicola Bonelli 
   All rights reserved. 

   Redistribution and use in source and binary forms, with or without 
   modification, are permitted provided that the following conditions are met: 

   * Redistributions of source code must retain the above copyright notice, 
     this list of conditions and the following disclaimer. 
   * Redistributions in binary form must reproduce the above copyright 
     notice, this list of conditions and the following disclaimer in the 
     documentation and/or other materials provided with the distribution. 
   * Neither the name of University of Pisa nor the names of its contributors 
     may be used to endorse or promote products derived from this software 
     without specific prior written permission. 

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
   POSSIBILITY OF SUCH DAMAGE.
 
 ***************************************************************/

#include <cstring>
#include <exception>

#include <pfq.hpp>

typedef void (*pfq_handler)(char *user, const struct pfq_hdr *h, const char *data); 

struct pfq_t : public net::pfq
{
    pfq_t()
    : net::pfq()
    , err(nullptr)
    {}

    template <typename ...Ts>
    pfq_t(Ts&& ... xs)
    : net::pfq(std::forward<Ts>(xs)...)
    , err(nullptr)
    {}

    ~pfq_t()
    {
        ::free(err);
    }

    mutable char *err;
};


template <typename Q, typename Fun>
auto firewall(int *ok, Q *q, Fun fun)
-> decltype(fun())
{   
    typedef decltype(fun()) result_type;
    try
    {
        * ok = 1;
        return fun();
    }
    catch(std::exception &e)
    {
        * ok = 0;
        ::free(q->err); q->err = strdup(e.what());
        return result_type(); 
    }
}


extern "C" {

    
    namespace 
    {
        __thread char * __error = nullptr;
    }

    /* costructor */

    pfq_t *pfq_default()        
    try
    {
        return new pfq_t;
    }
    catch(std::exception &e)
    {
        ::free(__error); __error = strdup(e.what());
        return nullptr;
    }

    pfq_t *pfq_open(size_t caplen, size_t offset, size_t slots)        
    try
    {
        return new pfq_t(caplen, offset, slots); 
    }
    catch(std::exception &e)
    {
        ::free(__error); __error = strdup(e.what());
        return nullptr;
    }

    void pfq_close(pfq_t *q)        
    {
        delete q;
    }

    const char *pfq_error(pfq_t *q)
    {
        return q ? q->err : __error;
    }

    void pfq_enable(pfq_t *q, int *ok)
    {
        firewall(ok, q, [&]() { q->enable(); });
    }

    void pfq_disable(pfq_t *q, int *ok)
    {
        firewall(ok, q, [&]() { q->disable(); });
    }

    int pfq_is_enabled(pfq_t const *q, int *ok)
    {
        return firewall(ok, q, [&]() { return q->is_enabled(); });
    }

    void pfq_load_balance(pfq_t *q, int value, int *ok)
    {
        firewall(ok, q, [&]() { q->load_balance(value); });
    }

    int pfq_ifindex(pfq_t const *q, const char *dev, int *ok)
    {
        return firewall(ok, q, [&]() { return q->ifindex(dev); });
    }

    void pfq_set_tstamp(pfq_t *q, int value, int *ok)
    {
        firewall(ok, q, [&]() { q->tstamp(value); });
    }

    int pfq_get_tstamp(pfq_t const *q, int *ok)
    {
        return firewall(ok, q, [&]() { return q->tstamp(); });
    }

    void pfq_set_caplen(pfq_t *q, size_t value, int *ok)
    {
        firewall(ok, q, [&]() { q->caplen(value); }); 
    }

    size_t pfq_get_caplen(pfq_t const *q, int *ok)
    {
        return firewall(ok, q, [&]() { return q->caplen(); }); 
    }
    
    void pfq_set_offset(pfq_t *q, size_t value, int *ok)
    {
        firewall(ok, q, [&]() { q->offset(value); }); 
    }

    size_t pfq_get_offset(pfq_t const *q, int *ok)
    {
        return firewall(ok, q, [&]() { return q->offset(); }); 
    }

    void pfq_set_slots(pfq_t *q, size_t value, int *ok)
    {
        firewall(ok, q, [&]() { q->slots(value); }); 
    }

    size_t pfq_get_slots(pfq_t const *q, int *ok)
    {
        return firewall(ok, q, [&]() { return q->slots(); }); 
    }
    
    size_t pfq_get_slot_size(pfq_t const *q, int *ok)
    {
        return firewall(ok, q, [&]() { return q->slot_size(); });
    }

    void pfq_add_device_by_index(pfq_t *q, int index, int queue, int *ok)
    {
        firewall(ok, q, [&]() { q->add_device(index,queue); });
    }

    void pfq_add_device_by_name(pfq_t *q, const char *dev, int queue, int *ok)
    {
        firewall(ok, q, [&]() { q->add_device(dev,queue); });
    }

    void pfq_remove_device_by_index(pfq_t *q, int index, int queue, int *ok)
    {
        firewall(ok, q, [&]() { q->remove_device(index,queue); }); 
    }

    void pfq_remove_device_by_name(pfq_t *q, const char *dev, int queue, int *ok)
    {
        firewall(ok, q, [&]() { q->remove_device(dev,queue); }); 
    }

    int pfq_poll(pfq_t *q, long int usec, int *ok)
    {
        return firewall(ok, q, [&]() { return q->poll(usec); }); 
    }

    int pfq_id(pfq_t const *q, int *ok)
    {
        return firewall(ok, q, [&]() { return q->id(); });
    }

    int pfq_fd(pfq_t const *q)
    {
        return q->fd();
    }

    struct pfq_stats
    pfq_get_stats(pfq_t const *q, int *ok)
    {
        return firewall(ok, q, [&]() { return q->stats(); });
    }
 
    int pfq_dispatch(pfq_t *q, pfq_handler callback, char *user, int *ok)
    {
        return firewall(ok, q, [&]() {
            
            auto many = q->read(100000); // usec
            
            auto it = std::begin(many),
                 it_e = std::end(many);
            int n = 0;
            for(; it != it_e; ++it)
            {
                while (!it->commit)
                    std::this_thread::yield();

                callback(user, &(*it), reinterpret_cast<const char *>(it.data()));
                n++;
            }
            return n;
        });
    }
}
