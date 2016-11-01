/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PFQ_PREFETCH_H
#define PFQ_PREFETCH_H

#include <linux/prefetch.h>

#ifdef PFQ_USE_PREFETCH

#define prefetch_w3(a)	__builtin_prefetch(a, 1, 3)
#define prefetch_w2(a)	__builtin_prefetch(a, 1, 2)
#define prefetch_w1(a)	__builtin_prefetch(a, 1, 1)
#define prefetch_w0(a)	__builtin_prefetch(a, 1, 0)

#define prefetch_r3(a)	__builtin_prefetch(a, 0, 3)
#define prefetch_r2(a)	__builtin_prefetch(a, 0, 2)
#define prefetch_r1(a)	__builtin_prefetch(a, 0, 1)
#define prefetch_r0(a)	__builtin_prefetch(a, 0, 0)

static inline void prefetchw_nt_range(void *addr, size_t len)
{
#ifdef ARCH_HAS_PREFETCH
	char *cp;
	char *end = addr + len;
	for (cp = addr; cp < end; cp += PREFETCH_STRIDE)
		prefetchw_nt(cp);
#endif
}

#else

#define prefetch_w3(a)
#define prefetch_w2(a)
#define prefetch_w1(a)
#define prefetch_w0(a)

#define prefetch_r3(a)
#define prefetch_r2(a)
#define prefetch_r1(a)
#define prefetch_r0(a)

static inline void prefetchw_nt_range(void *addr, size_t len)
{
}

#endif

#endif /* PFQ_SOCK_H */
