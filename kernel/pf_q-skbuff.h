/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#ifndef _PF_Q_SKBUFF_H_
#define _PF_Q_SKBUFF_H_

struct pergroup_context;
struct gc_log;

typedef struct
{
        unsigned long 	class_mask;
        uint32_t 	hash;
        uint8_t  	type;
        uint8_t  	attr;
        uint8_t  	direct;

} action_t;


struct pfq_cb
{
        action_t action;

        unsigned long group_mask;
        unsigned long state;

        struct pergroup_context *ctx;
	struct gc_log *log;

} __attribute__((packed));


#define PFQ_CB(skb) ((struct pfq_cb *)(skb)->cb)


#endif /* _PF_Q_SKBUFF_H_ */
