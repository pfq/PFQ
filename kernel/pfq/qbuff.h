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

#ifndef PF_Q_QBUFF_H
#define PF_Q_QBUFF_H

#include <pfq/vlan.h>
#include <pfq/types.h>

#include <linux/kernel.h>
#include <linux/version.h>
#include <linux/skbuff.h>


struct qbuff
{
	struct sk_buff *skb;
};


static inline uint16_t
qbuff_get_rx_queue(struct qbuff *buff)
{
	return skb_rx_queue_recorded(buff->skb) ? skb_get_rx_queue(buff->skb) : 0;
}


static inline bool
qbuff_run_bp_filter(struct qbuff *buff, struct pfq_group *this_group)
{
	struct sk_filter *bpf = (struct sk_filter *)atomic_long_read(&this_group->bp_filter);

	if (!bpf) return true;

#if (LINUX_VERSION_CODE < KERNEL_VERSION(3,15,0))
	return sk_run_filter(PFQ_SKB(buff), bpf->insns);
#elif (LINUX_VERSION_CODE < KERNEL_VERSION(4,4,0))
	return SK_RUN_FILTER(bpf, PFQ_SKB(buff));
#else
	return bpf_prog_run_save_cb(bpf->prog, PFQ_SKB(buff));
#endif

}


static inline bool
qbuff_run_vlan_filter(struct qbuff *buff, pfq_gid_t gid)
{
	return pfq_check_group_vlan_filter(gid, buff->skb->vlan_tci & ~VLAN_TAG_PRESENT);
}


#endif /* PF_Q_QBUFF_H */
