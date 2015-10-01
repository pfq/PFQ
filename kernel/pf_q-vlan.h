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


#ifndef PF_Q_VLAN_H
#define PF_Q_VLAN_H

#include <pragma/diagnostic_push>
#include <linux/version.h>
#include <linux/if_vlan.h>
#include <pragma/diagnostic_pop>

#if (LINUX_VERSION_CODE < KERNEL_VERSION(3,9,0))

extern struct sk_buff *pfq_vlan_untag(struct sk_buff *skb);

#elif (LINUX_VERSION_CODE < KERNEL_VERSION(3,16,6))

static inline
struct sk_buff *pfq_vlan_untag(struct sk_buff *skb)
{
        return vlan_untag(skb);
}

#else

static inline
struct sk_buff *pfq_vlan_untag(struct sk_buff *skb)
{
        return skb_vlan_untag(skb);
}

#endif

#endif /* PF_Q_VLAN_H */
