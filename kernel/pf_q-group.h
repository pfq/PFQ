/***************************************************************
 *                                                
 * (C) 2011-12 Nicola Bonelli <nicola.bonelli@cnit.it>   
 *             Andrea Di Pietro <andrea.dipietro@for.unipi.it>
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

#ifndef _PF_Q_GROUP_H_
#define _PF_Q_GROUP_H_ 

#define __PFQ_MODULE__
#include <linux/pf_q.h>

#include <sparse-counter.h>

struct pfq_group
{
    int pid;	/* process id for restricted join */;

	atomic_long_t ids;

	sparse_counter_t recv;
	sparse_counter_t lost;
	sparse_counter_t drop;
};


extern struct pfq_group pfq_groups[Q_MAX_GROUP];


static inline 
bool __pfq_has_joined(int gid, int id)
{
	return atomic_long_read(&pfq_groups[gid].ids) & (1L << id);
}


int pfq_join_free_group(int id, bool restricted);

int pfq_join_group(int gid, int id, bool restricted);

int pfq_leave_group(int gid, int id);

void pfq_leave_all_groups(int id);

unsigned long pfq_get_groups(int id);

#endif /* _PF_Q_GROUP_H_ */
