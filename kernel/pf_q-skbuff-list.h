/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PF_Q_SKBUFF_LIST_H
#define PF_Q_SKBUFF_LIST_H

#include <linux/skbuff.h>


#define PFQ_SK_BUFF_LIST_SIZE 16384
#define PFQ_SK_BUFF_LIST_MASK (PFQ_SK_BUFF_LIST_SIZE-1)


struct pfq_sk_buff_list
{
	struct sk_buff ** node;
	int head;
	int tail;
};


static inline
size_t pfq_sk_buff_list_size(struct pfq_sk_buff_list *list)
{
	size_t size = ((list->head - list->tail) + PFQ_SK_BUFF_LIST_SIZE) & PFQ_SK_BUFF_LIST_MASK;
	if (size == 0)
		return list->node[list->head] != NULL ? PFQ_SK_BUFF_LIST_SIZE : 0;
	return size;
}


static inline
int pfq_sk_buff_list_init (struct pfq_sk_buff_list *list)
{
	list->node = kzalloc(sizeof(struct sk_buff *) * PFQ_SK_BUFF_LIST_SIZE, GFP_KERNEL);
	if (list->node == NULL) {
		printk(KERN_INFO "[PFQ] pfq_sk_buff_list_init: out of memory!\n");
		return -ENOMEM;
	}

	list->head = 0;
	list->tail = 0;

	return 0;
}


static inline
void pfq_sk_buff_list_purge(struct pfq_sk_buff_list *list)
{
	int n;
	for(n = 0; list->node[n & PFQ_SK_BUFF_LIST_MASK]; n++)
	{
		kfree_skb(list->node[n & PFQ_SK_BUFF_LIST_MASK]);
		list->node[n & PFQ_SK_BUFF_LIST_MASK] = NULL;
	}
	list->head = 0;
	list->tail = 0;
}


static inline
void pfq_sk_buff_list_free(struct pfq_sk_buff_list *list)
{
	pfq_sk_buff_list_purge(list);
	kfree(list->node);
	list->node = NULL;
}


static inline
bool pfq_sk_buff_queue_head(struct pfq_sk_buff_list *list, struct sk_buff *skb)
{
	if (list->node[list->head] == NULL) {
        	list->node[list->head++] = skb;
        	list->head &= PFQ_SK_BUFF_LIST_MASK;
        	return true;
	}
	return false;
}


static inline
struct sk_buff *pfq_sk_buff_dequeue_tail(struct pfq_sk_buff_list *list)
{
	struct sk_buff *skb = list->node[list->tail];
	if (skb == NULL)
		return skb;
	list->node[list->tail++] = NULL;
        list->tail &= PFQ_SK_BUFF_LIST_MASK;
        return skb;
}


static inline
struct sk_buff *pfq_sk_buff_peek_tail(struct pfq_sk_buff_list *list)
{
	return list->node[list->tail];
}



#endif /* PF_Q_SKBUFF_LIST_H */
