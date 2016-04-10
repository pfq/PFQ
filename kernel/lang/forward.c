/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
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

#include <pragma/diagnostic_push>

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/cache.h>

#include <pragma/diagnostic_pop>

#include <lang/module.h>
#include <lang/forward.h>


#include <pf_q-skbuff.h>
#include <pf_q-transmit.h>
#include <pf_q-global.h>


static int
forward_init(arguments_t args)
{
	const char *name = GET_ARG(const char *, args);
	struct net_device *dev = dev_get_by_name(&init_net, name);
	if (dev == NULL) {
                printk(KERN_INFO "[PFQ|init] forward: %s no such device!\n", name);
                return -EINVAL;
	}

	/* override the address of the string (it's safe because its memory is owned by the group)... */

	SET_ARG(args, dev);
	printk(KERN_INFO "[PFQ|init] forward: device '%s' locked\n", dev->name);
	return 0;
}


static int
forward_fini(arguments_t args)
{
	struct net_device *dev = GET_ARG(struct net_device *, args);
	if (dev) {
		dev_put(dev);
		printk(KERN_INFO "[PFQ|fini] forward: device '%s' released\n", dev->name);
	}
	return 0;
}


static int
link_init(arguments_t args)
{
        const char **dev_name = GET_ARRAY(const char *, args);
	size_t n, ndev = LEN_ARRAY(args);

	for(n = 0; n < ndev; n++)
	{
		struct net_device *dev = dev_get_by_name(&init_net, dev_name[n]);
		if (dev == NULL) {
			printk(KERN_INFO "[PFQ|init] link: %s no such device!\n", dev_name[n]);
			dev_name[n] = NULL;
			continue;
		}

		dev_name[n] = (char *)SAFE_CAST(dev);
	}
	return 0;
}


static int
link_fini(arguments_t args)
{
        struct net_device **dev = GET_ARRAY(struct net_device *,args);
	size_t n, ndev = LEN_ARRAY(args);

	for(n = 0; n < ndev; n++)
	{
		if (dev[n]) {
			dev_put(dev[n]);
			printk(KERN_INFO "[PFQ|fini] forward: device '%s' released\n", dev[n]->name);
		}
	}
	return 0;
}


static ActionSkBuff
forwardIO(arguments_t args, SkBuff skb)
{
	struct net_device *dev = GET_ARG(struct net_device *, args);
	struct sk_buff *nskb;

	pfq_group_stats_t *stats = get_group_stats(skb);

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] forward: device error!\n");
		sparse_inc(&global_stats, disc);
		local_inc(&stats->disc);

                return Pass(skb);
	}

	nskb = skb_clone(PFQ_SKB(skb), GFP_ATOMIC);
	if (!nskb) {
                if (printk_ratelimit())
			printk(KERN_INFO "[pfq-lang] forward pfq_xmit %s: no memory!\n", dev->name);
		sparse_inc(&global_stats, disc);
		local_inc(&stats->disc);
		return Pass(skb);
	}

	if (pfq_xmit(nskb, dev, nskb->queue_mapping, 0) != 1) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] forward pfq_xmit: error on device %s!\n", dev->name);

		sparse_inc(&global_stats, disc);
		local_inc(&stats->disc);
	}
	else {
		sparse_inc(&global_stats, frwd);
		local_inc(&stats->frwd);
	}

	return Pass(skb);
}


static ActionSkBuff
forward(arguments_t args, SkBuff skb)
{
	struct net_device *dev = GET_ARG(struct net_device *, args);
        pfq_group_stats_t *stats;

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] forward: device error!\n");
                return Pass(skb);
	}

	pfq_lazy_xmit(skb, dev, skb->queue_mapping);

	stats = get_group_stats(skb);
	local_inc(&stats->frwd);

	return Pass(skb);
}


static ActionSkBuff
bridge(arguments_t args, SkBuff skb)
{
	struct net_device *dev = GET_ARG(struct net_device *, args);

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] bridge: device error!\n");
                return Drop(skb);
	}

	pfq_lazy_xmit(skb, dev, skb->queue_mapping);

	local_inc(&get_group_stats(skb)->frwd);

	return Drop(skb);
}


static ActionSkBuff
link(arguments_t args, SkBuff skb)
{
        struct net_device **dev = GET_ARRAY(struct net_device *,args);
	size_t n, ndev = LEN_ARRAY(args);
        pfq_group_stats_t *stats = get_group_stats(skb);

	for(n = 0; n < ndev; n++)
	{
		if (dev[n] != NULL && skb->dev != dev[n])
		{
			pfq_lazy_xmit(skb, dev[n], skb->queue_mapping);
			local_inc(&stats->frwd);
		}
	}

	return Pass(skb);
}


static ActionSkBuff
tap(arguments_t args, SkBuff skb)
{
	struct net_device *dev = GET_ARG(struct net_device *, args);
	predicate_t pred_  = GET_ARG_1(predicate_t, args);

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] tap: device error!\n");
                return Drop(skb);
	}

        if (EVAL_PREDICATE(pred_, skb))
		return Pass(skb);

	pfq_lazy_xmit(skb, dev, skb->queue_mapping);

	local_inc(&get_group_stats(skb)->frwd);

	return Drop(skb);
}


static ActionSkBuff
tee(arguments_t args, SkBuff skb)
{
	struct net_device *dev = GET_ARG(struct net_device *, args);
	predicate_t pred_  = GET_ARG_1(predicate_t, args);

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] tee: device error!\n");
                return Drop(skb);
	}

	pfq_lazy_xmit(skb, dev, skb->queue_mapping);

	local_inc(&get_group_stats(skb)->frwd);

        if (EVAL_PREDICATE(pred_, skb))
		return Pass(skb);

	return Drop(skb);
}


struct pfq_lang_function_descr forward_functions[] = {

        { "drop",       "SkBuff -> Action SkBuff",		forward_drop		},
        { "broadcast",  "SkBuff -> Action SkBuff",		forward_broadcast	},
        { "classify",	"CInt -> SkBuff -> Action SkBuff",	forward_class		},
        { "kernel",	"SkBuff -> Action SkBuff",		forward_to_kernel	},

	{ "forwardIO",  "String -> SkBuff -> Action SkBuff",			 forwardIO, forward_init, forward_fini },
	{ "forward",    "String -> SkBuff -> Action SkBuff",			 forward,   forward_init, forward_fini },
	{ "link",	"[String] -> SkBuff -> Action SkBuff",			 link,	    link_init,    link_fini    },

	{ "bridge",     "String -> SkBuff -> Action SkBuff",			 bridge,    forward_init, forward_fini },
	{ "tee",	"String -> (SkBuff -> Bool) -> SkBuff -> Action SkBuff", tee,	    forward_init, forward_fini },
	{ "tap",	"String -> (SkBuff -> Bool) -> SkBuff -> Action SkBuff", tap,	    forward_init, forward_fini },

        { NULL }};

