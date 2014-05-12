/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <linux/kernel.h>
#include <linux/module.h>

#include <pf_q-engine.h>
#include <pf_q-transmit.h>
#include <pf_q-module.h>

#include "forward.h"

static struct sk_buff *
sink(arguments_t args, struct sk_buff *skb)
{
        if (!is_stolen(skb))
        {
                kfree_skb(skb);
        }
        return NULL;
}


static struct sk_buff *
forward(arguments_t args, struct sk_buff *skb)
{
        const int index = get_data(int, args);

	struct net_device *dev = dev_get_by_index(&init_net, index);
	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] forward: device error!\n");
                return skb;
	}

	atomic_inc(&skb->users);

	if (pfq_xmit(skb, dev, skb->queue_mapping) != 1) {
#ifdef DEBUG
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] forward pfq_xmit: error on device %s!\n", dev->name);
#endif
	}

	dev_put(dev);
	return skb;
}

static int
forward_init(arguments_t args)
{
	printk(KERN_INFO "[PFQ] %s\n", __PRETTY_FUNCTION__);
	return 0;
}

static int
forward_fini(arguments_t args)
{
	printk(KERN_INFO "[PFQ] %s\n", __PRETTY_FUNCTION__);
	return 0;
}


struct pfq_monadic_fun_descr forward_functions[] = {

        { "drop",       FUN_ACTION, 			INLINE_FUN(forward_drop)   	},
        { "broadcast",  FUN_ACTION, 			INLINE_FUN(forward_broadcast)	},
        { "kernel",     FUN_ACTION, 			INLINE_FUN(forward_kernel)   	},
        { "class",	FUN_ACTION | FUN_ARG_DATA, 	INLINE_FUN(forward_class) 	},

        { "sink",       FUN_ACTION, 			sink },
	{ "forward",    FUN_ACTION | FUN_ARG_DATA, 	forward, forward_init, forward_fini },

        { NULL }};

