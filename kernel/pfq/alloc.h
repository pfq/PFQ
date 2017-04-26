/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
 * 	       Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>
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

#ifndef PFQ_ALLOC_H
#define PFQ_ALLOC_H

#include <linux/gfp.h>

inline static
void *pfq_malloc_pages(size_t size, gfp_t gfp_flags)
{
	int po;
	if (WARN_ON(!size))
		return NULL;
	gfp_flags |= __GFP_COMP;
	po = get_order(size);
	return (void *) __get_free_pages(gfp_flags, po);
}


inline static
void pfq_free_pages(void *addr, size_t size)
{
	int po;
	if (addr == NULL)
	       return;
	po = get_order(size);
        free_pages((unsigned long) addr, po);
}

#endif /* PFQ_ALLOC_H */

