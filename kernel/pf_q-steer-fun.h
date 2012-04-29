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

#ifndef _PF_Q_STEER_FUN_H_
#define _PF_Q_STEER_FUN_H_ 

#include <linux/skbuff.h>

#define STEER_OK(x) ((x) == 0 ? 1 : (x))
#define STEER_FAIL	0

extern unsigned long steer_mac_addr(const struct sk_buff *skb);
extern unsigned long steer_vlan_untag(const struct sk_buff *skb);
extern unsigned long steer_vlan_id(const struct sk_buff *skb);
extern unsigned long steer_ipv4_addr(const struct sk_buff *skb);
extern unsigned long steer_ipv6_addr(const struct sk_buff *skb);

#endif /* _PF_Q_STEER_FUN_H_ */
