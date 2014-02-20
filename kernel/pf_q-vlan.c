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

#include <linux/if_vlan.h>
#include <linux/version.h>

/* inspired to linux kernel vlan_untag */

#if (LINUX_VERSION_CODE < KERNEL_VERSION(3,9,0))

static inline
void pfq_vlan_set_encap_proto(struct sk_buff *skb, struct vlan_hdr *vhdr)
{
        __be16 proto;
        unsigned char *rawp;

        /*
         * Was a VLAN packet, grab the encapsulated protocol, which the layer
         * three protocols care about.
         */

        proto = vhdr->h_vlan_encapsulated_proto;
        if (ntohs(proto) >= 1536) {
                skb->protocol = proto;
                return;
        }

        rawp = skb->data;
        if (*(unsigned short *) rawp == 0xFFFF)
                /*
                 * This is a magic hack to spot IPX packets. Older Novell
                 * breaks the protocol design and runs IPX over 802.3 without
                 * an 802.2 LLC layer. We look for FFFF which isn't a used
                 * 802.2 SSAP/DSAP. This won't work for fault tolerant netware
                 * but does for the rest.
                 */
                skb->protocol = htons(ETH_P_802_3);
        else
                /*
                 * Real 802.2 LLC
                 */
                skb->protocol = htons(ETH_P_802_2);
}


static inline
struct sk_buff *
pfq_vlan_reorder_header(struct sk_buff *skb)
{
        if (skb_cow(skb, skb_headroom(skb)) < 0)
                return NULL;
        memmove(skb->data - ETH_HLEN, skb->data - VLAN_ETH_HLEN, 2 * ETH_ALEN);
        skb->mac_header += VLAN_HLEN;
        skb_reset_mac_len(skb);
        return skb;
}


struct sk_buff *pfq_vlan_untag(struct sk_buff *skb)
{
        struct vlan_hdr *vhdr;
        u16 vlan_tci;

        if (unlikely(vlan_tx_tag_present(skb))) {
                /* vlan_tci is already set-up so leave this for another time */
                return skb;
        }

        if (unlikely(!pskb_may_pull(skb, VLAN_HLEN)))
                goto err_free;

        vhdr = (struct vlan_hdr *) skb->data;
        vlan_tci = ntohs(vhdr->h_vlan_TCI);

        __vlan_hwaccel_put_tag(skb, vlan_tci);

        skb_pull_rcsum(skb, VLAN_HLEN);
        pfq_vlan_set_encap_proto(skb, vhdr);

        skb = pfq_vlan_reorder_header(skb);
        if (unlikely(!skb))
                goto err_free;

        skb_reset_network_header(skb);
        skb_reset_transport_header(skb);
        return skb;

err_free:
        kfree_skb(skb);
        return NULL;
}


#endif



