/***************************************************************
 *                                                
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>   
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * The full GNU General Public License is included in this distribution in
 * the file called "COPYING".
 *
 ****************************************************************/

#ifndef _PF_Q_STEER_FUN_H_
#define _PF_Q_STEER_FUN_H_ 

#include <linux/pf_q.h>

#include <linux/skbuff.h>
#include <linux/ip.h>
#include <linux/ipv6.h>
#include <linux/if_ether.h>
#include <linux/if_vlan.h>


typedef struct
{
	unsigned int  hash:24;
	unsigned int  type:8;
	unsigned int  class;
} steering_t;


enum action 
{
    action_drop      = 0x01,
    action_clone     = 0x02,
    action_dispatch  = 0x04,
    action_steal     = 0x08,
    action_continue  = 0x10,
    action_to_kernel = 0x80,
};


typedef steering_t (*steering_function_t)(const struct sk_buff *, const void *);    


struct steering_function
{
    const char *            name;
    steering_function_t     function;
};


extern int pfq_register_steering_functions  (const char *module, struct steering_function *fun);
extern int pfq_unregister_steering_functions(const char *module, struct steering_function *fun);


/* none: ignore the packet for the current group */

static inline
steering_t none(void)
{
    steering_t ret = { 0, action_drop, 0};
    return ret;
}

/* broadcast: for this group, broadcast the skb to sockets of the given classes */

static inline
steering_t broadcast(unsigned int cl)
{
    steering_t ret = { 0, action_clone, cl};
    return ret;
}


/* steering skb: for this group, dispatch the skb across sockets of the given classes (by means of hash) */

static inline
steering_t steering(unsigned int cl, unsigned int hash)
{
    steering_t ret = {hash ^ (hash >> 8), action_dispatch, cl};
    return ret;
}


/* stolen packet: the skb is stolen by the steering function. (i.e. forwarded) */

static inline
steering_t stolen(void)
{
    steering_t ret = { 0, action_steal, 0};
    return ret;
}


/* modifier */

/* to_kernel: set the skb to be passed to kernel */

static inline
steering_t to_kernel(steering_t ret)
{
    if (unlikely(ret.type & action_steal))
    {
        if (printk_ratelimit())
 	        pr_devel("[PFQ] to_kernel modifier applied to a stolen packet!\n");
       return ret;
    }
    ret.type |= action_to_kernel;
    return ret;
}


#endif /* _PF_Q_STEER_FUN_H_ */
