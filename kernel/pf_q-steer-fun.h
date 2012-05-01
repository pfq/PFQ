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

#include <linux/pf_q.h>
#include <linux/skbuff.h>
 

typedef struct
{
	int type;
	unsigned int hash;
} steer_ret_t;


typedef steer_ret_t (*steer_function_t)(const struct sk_buff *);


enum {
    hash_clone = 1,
    hash_bound = 2
};


static inline
steer_ret_t steer_none(void)
{
    steer_ret_t ret = { 0, 0 };
    return ret;
}


static inline
steer_ret_t steer_data(unsigned long hash)
{
    steer_ret_t ret = { Q_GROUP_DATA, (hash < hash_bound ? hash + hash_bound : hash) };
    return ret;
}
static inline
steer_ret_t clone_data(void)
{
    steer_ret_t ret = { Q_GROUP_DATA, hash_clone };
    return ret;
}



static inline
steer_ret_t steer_control(unsigned long hash)
{
    steer_ret_t ret = { Q_GROUP_CONTROL, (hash < hash_bound ? hash + hash_bound : hash) };
    return ret;
}
static inline
steer_ret_t clone_control(void)
{
    steer_ret_t ret = { Q_GROUP_CONTROL, hash_clone };
    return ret;
}


static inline
steer_ret_t steer_out_of_band(unsigned long hash)
{
    steer_ret_t ret = { Q_GROUP_OUT_OF_BAND, (hash < hash_bound ? hash + hash_bound : hash) };
    return ret;
}
static inline
steer_ret_t clone_out_of_band(void)
{
    steer_ret_t ret = { Q_GROUP_OUT_OF_BAND, hash_clone };
    return ret;
}


extern steer_ret_t steer_mac_addr(const struct sk_buff *skb);
extern steer_ret_t steer_vlan_untag(const struct sk_buff *skb);
extern steer_ret_t steer_vlan_id(const struct sk_buff *skb);
extern steer_ret_t steer_ipv4_addr(const struct sk_buff *skb);
extern steer_ret_t steer_ipv6_addr(const struct sk_buff *skb);

#endif /* _PF_Q_STEER_FUN_H_ */
