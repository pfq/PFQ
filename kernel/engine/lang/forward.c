/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#include <engine/lang/forward.h>
#include <engine/global.h>
#include <engine/percpu.h>
#include <engine/io.h>

#include <pfq/netdev.h>

static int
forward_init(arguments_t args)
{
	const char *name = GET_ARG(const char *, args);
	struct net_device *dev = pfq_dev_get_by_name(name);
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
		pfq_dev_put(dev);
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
		struct net_device *dev = pfq_dev_get_by_name(dev_name[n]);
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
			pfq_dev_put(dev[n]);
			printk(KERN_INFO "[PFQ|fini] forward: device '%s' released\n", dev[n]->name);
		}
	}
	return 0;
}


static ActionQbuff
forwardIO(arguments_t args, struct qbuff * buff)
{
	struct net_device *dev = GET_ARG(struct net_device *, args);
	struct qbuff *nbuff;

	pfq_group_stats_t *stats = get_group_stats(buff);

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] forward: device error!\n");
		sparse_inc(global_stats, disc);
		local_inc(&stats->disc);

                return Pass(buff);
	}

	nbuff = qbuff_clone(buff);
	if (!nbuff) {
                if (printk_ratelimit())
			printk(KERN_INFO "[pfq-lang] forward pfq_xmit %s: no memory!\n", dev->name);
		sparse_inc(global_stats, disc);
		local_inc(&stats->disc);
		return Pass(buff);
	}

	if (pfq_xmit(nbuff, dev, qbuff_get_queue_mapping(nbuff), 0) != 1) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] forward pfq_xmit: error on device %s!\n", dev->name);

		sparse_inc(global_stats, disc);
		local_inc(&stats->disc);
	}
	else {
		sparse_inc(global_stats, frwd);
		local_inc(&stats->frwd);
	}

	return Pass(buff);
}


static ActionQbuff
forward(arguments_t args, struct qbuff * buff)
{
	struct net_device *dev = GET_ARG(struct net_device *, args);
        pfq_group_stats_t *stats;

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] forward: device error!\n");
                return Pass(buff);
	}

	pfq_lazy_xmit(buff, dev, qbuff_get_queue_mapping(buff));

	stats = get_group_stats(buff);
	local_inc(&stats->frwd);

	return Pass(buff);
}


static ActionQbuff
bridge(arguments_t args, struct qbuff * buff)
{
	struct net_device *dev = GET_ARG(struct net_device *, args);

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] bridge: device error!\n");
                return Drop(buff);
	}

	pfq_lazy_xmit(buff, dev, qbuff_get_queue_mapping(buff));

	local_inc(&get_group_stats(buff)->frwd);

	return Drop(buff);
}


static ActionQbuff
link(arguments_t args, struct qbuff * buff)
{
        struct net_device **dev = GET_ARRAY(struct net_device *,args);
	size_t n, ndev = LEN_ARRAY(args);

        pfq_group_stats_t *stats = get_group_stats(buff);

	for(n = 0; n < ndev; n++)
	{
		if (dev[n] != NULL && qbuff_device(buff) != dev[n])
		{
			pfq_lazy_xmit(buff, dev[n], qbuff_get_queue_mapping(buff));
			local_inc(&stats->frwd);
		}
	}

	return Pass(buff);
}


static ActionQbuff
tap(arguments_t args, struct qbuff * buff)
{
	struct net_device *dev = GET_ARG(struct net_device *, args);
	predicate_t pred_  = GET_ARG_1(predicate_t, args);

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] tap: device error!\n");
                return Drop(buff);
	}

        if (EVAL_PREDICATE(pred_, buff))
		return Pass(buff);

	pfq_lazy_xmit(buff, dev, qbuff_get_queue_mapping(buff));

	local_inc(&get_group_stats(buff)->frwd);

	return Drop(buff);
}


static ActionQbuff
tee(arguments_t args, struct qbuff * buff)
{
	struct net_device *dev = GET_ARG(struct net_device *, args);
	predicate_t pred_  = GET_ARG_1(predicate_t, args);

	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] tee: device error!\n");
                return Drop(buff);
	}

	pfq_lazy_xmit(buff, dev, qbuff_get_queue_mapping(buff));

	local_inc(&get_group_stats(buff)->frwd);

        if (EVAL_PREDICATE(pred_, buff))
		return Pass(buff);

	return Drop(buff);
}


static ActionQbuff
forward_if_kernel(arguments_t args, struct qbuff * b)
{
        predicate_t pred_ = GET_ARG_0(predicate_t, args);
        if (EVAL_PREDICATE(pred_, b))
		to_kernel(b);
        return Pass(b);
}


static ActionQbuff
detour_if_kernel(arguments_t args, struct qbuff * b)
{
        predicate_t pred_ = GET_ARG_0(predicate_t, args);
        if (EVAL_PREDICATE(pred_, b))
		return Drop(to_kernel(b));
	else
		return Pass(b);
}


struct pfq_lang_function_descr forward_functions[] = {

        { "drop",       "Qbuff -> Action Qbuff",		forward_drop	   , NULL, NULL     },
        { "broadcast",  "Qbuff -> Action Qbuff",		forward_broadcast  , NULL, NULL     },
        { "classify",	"CInt -> Qbuff -> Action Qbuff",	forward_class	   , NULL, NULL     },
        { "kernel",	"Qbuff -> Action Qbuff",		forward_kernel	   , NULL, NULL     },
        { "detour",	"Qbuff -> Action Qbuff",		detour_kernel	   , NULL, NULL     },

        { "kernel_if",	"(Qbuff -> Bool) -> Qbuff -> Action Qbuff",	forward_if_kernel  , NULL, NULL     },
        { "detour_if",	"(Qbuff -> Bool) -> Qbuff -> Action Qbuff",	detour_if_kernel   , NULL, NULL     },


	{ "forwardIO",  "String -> Qbuff -> Action Qbuff",			 forwardIO, forward_init, forward_fini },
	{ "forward",    "String -> Qbuff -> Action Qbuff",			 forward,   forward_init, forward_fini },
	{ "link",	"[String] -> Qbuff -> Action Qbuff",			 link_,	    link_init,    link_fini    },

	{ "bridge",     "String -> Qbuff -> Action Qbuff",			 bridge,    forward_init, forward_fini },
	{ "tee",	"String -> (Qbuff -> Bool) -> Qbuff -> Action Qbuff", tee,	    forward_init, forward_fini },
	{ "tap",	"String -> (Qbuff -> Bool) -> Qbuff -> Action Qbuff", tap,	    forward_init, forward_fini },

        { NULL }};

