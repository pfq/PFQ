/***************************************************************
 *                                                
 * (C) 2011-12 Nicola Bonelli <nicola.bonelli@cnit.it>   
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

#include <linux/skbuff.h>
#include <linux/ip.h>
#include <linux/if_ether.h>
#include <linux/if_vlan.h>


unsigned long
steer_vlan_id(const struct sk_buff *skb)
{
 	return skb->vlan_tci & VLAN_VID_MASK;
}


unsigned long 
steer_ipv4_addr(const struct sk_buff *skb)
{ 
        struct ethhdr *eth;

        eth = (struct ethhdr *)skb_mac_header(skb);
        if (eth->h_proto != __constant_htons(ETH_P_IP)) {
                return 0;
        }

        return  ip_hdr(skb)->saddr ^ ip_hdr(skb)->daddr;
}


