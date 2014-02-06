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

#include <pf_q-common.h>

int pfq_tx_ring_skb_alloc(struct pfq_tx_opt *tq, size_t n)
{
        struct sk_buff * skb;

        skb = __dev_alloc_skb(1518, GFP_KERNEL);
        if (skb == NULL)
                return -ENOMEM;

        /* save the skb in the ring */
        tq->skb_slot[n & PFQ_TX_RING_MASK] = skb;
        return 0;
}

void pfq_tx_ring_skb_free(struct pfq_tx_opt *tq, size_t n)
{
        struct sk_buff *skb = tq->skb_slot[n & PFQ_TX_RING_MASK];
        if(skb)
        {
                while(skb_shared(skb))
                        msleep(1);

                kfree_skb(skb);
                tq->skb_slot[n & PFQ_TX_RING_MASK] = NULL;
        }
}

