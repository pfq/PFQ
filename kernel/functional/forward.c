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
#include <linux/cache.h>

#include <pf_q-transmit.h>
#include <pf_q-module.h>
#include <pf_q-non-intrusive.h>
#include <pf_q-global.h>

#include "forward.h"

struct forward_queue
{
	struct pfq_non_intrusive_skb q;

} ____chaline_aligned;


static struct sk_buff *
forward(arguments_t args, struct sk_buff *skb)
{
	struct net_device *dev = get_data(struct net_device *, args);
	struct sk_buff *nskb;

#ifdef PFQ_USE_BATCH_FORWARD

       	struct forward_queue *queues;
       	int id;

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] forward: device error!\n");
                return skb;
	}

       	queues = get_data2(struct forward_queue *, args);

#ifdef PFQ_NOCLONE_FORWARD
	nskb = atomic_inc(&skb->users);
#else
	nskb = skb_clone(skb, GFP_ATOMIC);
	if (!nskb) {
        	printk(KERN_INFO "[PFQ] forward pfq_xmit %s: no memory!\n", dev->name);
        	return skb;
	}
#endif

	id = smp_processor_id();

	pfq_non_intrusive_push(&queues[id].q, nskb);

	if (pfq_non_intrusive_len(&queues[id].q) < batch_len) {
		return skb;
	}

	if (pfq_queue_xmit(&queues[id].q, dev, id) == 0) {
#ifdef DEBUG
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] forward pfq_queue_xmit: error on device %s!\n", dev->name);
#endif
	}

	pfq_non_intrusive_flush(&queues[id].q);

#else /* PFQ_USE_BATCH_FORWARD */

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] forward: device error!\n");
                return skb;
	}

#ifdef PFQ_NOCLONE_FORWARD
	nskb = atomic_inc(&skb->users);
#else
	nskb = skb_clone(skb, GFP_ATOMIC);
	if (!nskb) {
        	printk(KERN_INFO "[PFQ] forward pfq_xmit %s: no memory!\n", dev->name);
        	return skb;
	}
#endif

	if (pfq_xmit(nskb, dev, nskb->queue_mapping) != 1) {
#ifdef DEBUG
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] forward pfq_xmit: error on device %s!\n", dev->name);
#endif
	}

#endif /* PFQ_USE_BATCH_FORWARD */

	return skb;
}


static int
forward_init(arguments_t args)
{
	const char *name = get_data(const char *, args);

	struct net_device *dev = dev_get_by_name(&init_net, name);

	if (dev == NULL) {
                printk(KERN_INFO "[PFQ|init] forward: %s no such device!\n", name);
                return -1;
	}

	/* it is safe to override the address of the string... */

	set_data(args, dev);

	pr_devel("[PFQ|init] forward: device '%s' locked.\n", dev->name);

#ifdef PFQ_USE_BATCH_FORWARD
	{
       		struct forward_queue *queues;
		int n;

		queues = kmalloc(sizeof(struct forward_queue) * 64, GFP_KERNEL);
		if (!queues) {
			printk(KERN_INFO "[PFQ|init] forward: out of memory!\n");
			return -1;
		}

		for(n = 0; n < 64; n++)
		{
			pfq_non_intrusive_init(&queues[n].q);
		}

		pr_devel("[PFQ|init] forward: queues initialized.\n");

		set_data2(args, queues);
	}
#endif

	return 0;
}


static int
forward_fini(arguments_t args)
{
	struct net_device *dev = get_data(struct net_device *, args);

	if (dev)
	{
		dev_put(dev);
		pr_devel("[PFQ|fini] forward: device '%s' released.\n", dev->name);
	}

#ifdef PFQ_USE_BATCH_FORWARD
	{
       		struct forward_queue *queues = get_data2(struct forward_queue *, args);
        	struct sk_buff *skb;
       		int n, i;

		for(n = 0; n < 64; n++)
		{
			pfq_non_intrusive_for_each(skb, i, &queues[n].q)
			{
				kfree_skb(skb);
			}
		}

		kfree(queues);

		pr_devel("[PFQ|fini] forward: queues freed.\n");
	}
#endif
	return 0;
}


struct pfq_function_descr forward_functions[] = {

        { "drop",        "SkBuff -> Action SkBuff",   	    		INLINE_FUN(forward_drop) 	},
        { "broadcast",   "SkBuff -> Action SkBuff",   	    		INLINE_FUN(forward_broadcast)	},
        { "class",	 "Int -> SkBuff -> Action SkBuff",  		INLINE_FUN(forward_class) 	},
        { "deliver",	 "Int -> SkBuff -> Action SkBuff",  		INLINE_FUN(forward_deliver) 	},

        { "kernel", 	    "SkBuff -> Action SkBuff",    		INLINE_FUN(forward_to_kernel)       },
        { "forward_kernel", "SkBuff -> Action SkBuff",   		INLINE_FUN(forward_io_kernel)       },
	{ "forward",        "String -> SkBuff -> Action SkBuff",  	forward, forward_init, forward_fini },

        { NULL }};

