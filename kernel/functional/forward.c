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

#include <linux/kernel.h>
#include <linux/module.h>

#include <linux/pf_q-module.h>

#include <pf_q-transmit.h>


static struct sk_buff *
sink(argument_t a, struct sk_buff *skb)
{
        if (!is_stolen(skb))
        {
                kfree_skb(skb);
        }
        return NULL;
}


static struct sk_buff *
forward(argument_t a, struct sk_buff *skb)
{
        int *index = argument_as(int, a);

	struct net_device *dev = dev_get_by_index(&init_net, *index);
	if (dev == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] forward: device error!\n");
                return skb;
	}

	atomic_inc(&skb->users);

	if (pfq_xmit(skb, dev, skb->queue_mapping) != 1) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] forward: pfq_xmit error!\n");
	}

	dev_put(dev);
	return skb;
}


struct pfq_monadic_fun_descr forward_functions[] = {

        { "sink",               sink 		},
	{ "forward", 		forward 	},

        { NULL, NULL}};

