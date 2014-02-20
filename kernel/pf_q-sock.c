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

#include <linux/kernel.h>
#include <linux/module.h>

#include <linux/version.h>
#include <linux/types.h>

#include <pf_q-sock.h>

/* vector of pointers to pfq_sock */

atomic_long_t pfq_sock_vector[Q_MAX_ID];


int pfq_get_free_sock_id(struct pfq_sock * so)
{
        int n = 0;
        for(; n < Q_MAX_ID; n++)
        {
                if (!atomic_long_cmpxchg(pfq_sock_vector + n, 0, (long)so))
                        return n;
        }
        return -1;
}


struct pfq_sock * pfq_get_sock_by_id(size_t id)
{
        struct pfq_sock * so;
        if (unlikely(id >= Q_MAX_ID))
        {
                pr_devel("[PFQ] pfq_devmap_freeid: bad id=%zd!\n", id);
                return NULL;
        }
	so = (struct pfq_sock *)atomic_long_read(&pfq_sock_vector[id]);
	smp_read_barrier_depends();
	return so;
}


void pfq_release_sock_id(int id)
{
        if (unlikely(id >= Q_MAX_ID || id < 0))
        {
                pr_devel("[PFQ] pfq_devmap_freeid: bad id=%d!\n", id);
                return;
        }
        atomic_long_set(pfq_sock_vector + id, 0);
}


