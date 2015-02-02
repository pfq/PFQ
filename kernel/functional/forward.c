/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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

#include <pf_q-skbuff-batch.h>
#include <pf_q-transmit.h>
#include <pf_q-module.h>
#include <pf_q-global.h>


#include "forward.h"


struct forward_queue
{
	struct pfq_skbuff_short_batch q;

} ____chaline_aligned;


static Action_SkBuff
forwardIO(arguments_t args, SkBuff b)
{
	struct net_device *dev = get_arg(struct net_device *, args);
	struct sk_buff *nskb;
	struct pfq_group_stats *stats = get_stats(b);

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ/lang] forward: device error!\n");
		sparse_inc(&global_stats.quit);
		sparse_inc(&stats->quit);
                return Pass(b);
	}

	nskb = skb_clone(b.skb, GFP_ATOMIC);
	if (!nskb) {
                if (printk_ratelimit())
        		printk(KERN_INFO "[PFQ/lang] forward pfq_xmit %s: no memory!\n", dev->name);
		sparse_inc(&global_stats.quit);
		sparse_inc(&stats->quit);
        	return Pass(b);
	}

	if (pfq_xmit(nskb, dev, nskb->queue_mapping, 0) != 1) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ/lang] forward pfq_xmit: error on device %s!\n", dev->name);

		sparse_inc(&global_stats.disc);
		sparse_inc(&stats->disc);
	}
	else {
		sparse_inc(&global_stats.frwd);
		sparse_inc(&stats->frwd);
	}

	return Pass(b);
}


static Action_SkBuff
forward(arguments_t args, SkBuff b)
{
	struct net_device *dev = get_arg(struct net_device *, args);

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ/lang] forward: device error!\n");
                return Pass(b);
	}

	pfq_lazy_xmit(b, dev, b.skb->queue_mapping);

	sparse_inc(&get_stats(b)->frwd);

	return Pass(b);
}


static int
forward_init(arguments_t args)
{
	const char *name = get_arg(const char *, args);
	struct net_device *dev = dev_get_by_name(&init_net, name);

	if (dev == NULL) {
                printk(KERN_INFO "[PFQ|init] forward: %s no such device!\n", name);
                return -EINVAL;
	}

	/* it is safe to override the address of the string... */

	set_arg(args, dev);

	printk(KERN_INFO "[PFQ|init] forward: device '%s' locked\n", dev->name);
	return 0;
}


static int
forward_fini(arguments_t args)
{
	struct net_device *dev = get_arg(struct net_device *, args);

	if (dev)
	{
		dev_put(dev);
		printk(KERN_INFO "[PFQ|fini] forward: device '%s' released\n", dev->name);
	}

	return 0;
}


static Action_SkBuff
bridge(arguments_t args, SkBuff b)
{
	struct net_device *dev = get_arg(struct net_device *, args);

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ/lang] bridge: device error!\n");
                return Drop(b);
	}

	pfq_lazy_xmit(b, dev, b.skb->queue_mapping);

	sparse_inc(&get_stats(b)->frwd);

	return Drop(b);
}


static Action_SkBuff
tap(arguments_t args, SkBuff b)
{
	struct net_device *dev = get_arg(struct net_device *, args);
	predicate_t pred_  = get_arg1(predicate_t, args);

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ/lang] bridge: device error!\n");
                return Drop(b);
	}

        if (EVAL_PREDICATE(pred_, b))
		return Pass(b);

	pfq_lazy_xmit(b, dev, b.skb->queue_mapping);

	sparse_inc(&get_stats(b)->frwd);

	return Drop(b);
}


static Action_SkBuff
tee(arguments_t args, SkBuff b)
{
	struct net_device *dev = get_arg(struct net_device *, args);
	predicate_t pred_  = get_arg1(predicate_t, args);

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ/lang] bridge: device error!\n");
                return Drop(b);
	}

	pfq_lazy_xmit(b, dev, b.skb->queue_mapping);

	sparse_inc(&get_stats(b)->frwd);

        if (EVAL_PREDICATE(pred_, b))
		return Pass(b);

	return Drop(b);
}


struct pfq_function_descr forward_functions[] = {

        { "drop",       "SkBuff -> Action SkBuff",   	    		forward_drop		},
        { "broadcast",  "SkBuff -> Action SkBuff",   	    		forward_broadcast	},
        { "class",	"CInt -> SkBuff -> Action SkBuff",  		forward_class		},
        { "deliver",	"CInt -> SkBuff -> Action SkBuff",  		forward_deliver		},
        { "kernel",    	"SkBuff -> Action SkBuff",    			forward_to_kernel 	},

	{ "forwardIO",  "String -> SkBuff -> Action SkBuff",  		forwardIO,  forward_init, forward_fini },
	{ "forward",    "String -> SkBuff -> Action SkBuff",  		forward,    forward_init, forward_fini },

	{ "bridge",     "String -> SkBuff -> Action SkBuff",  			 bridge, forward_init, forward_fini },
	{ "tee", 	"String -> (SkBuff -> Bool) -> SkBuff -> Action SkBuff", tee, 	 forward_init, forward_fini },
	{ "tap", 	"String -> (SkBuff -> Bool) -> SkBuff -> Action SkBuff", tap, 	 forward_init, forward_fini },

        { NULL }};

