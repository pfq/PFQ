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

#ifndef _FUNCTIONAL_BLOOM_H_
#define _FUNCTIONAL_BLOOM_H_

#include <pf_q-module.h>

/* macros to test/set bits in bitwise array */

#define BF_TEST(mem, x)  (mem[(x)>>3] &  (1<<((x) & 7)))

#define BF_SET(mem, x)   (mem[(x)>>3] |= (1<<((x) & 7)))


static inline uint32_t hfun1(uint32_t ip_)
{
	return ip_ & 0x00ffffff;
}

static inline uint32_t hfun2(uint32_t ip_)
{
	return (ip_ & 0xffffff00) >> 8;
}

static inline uint32_t hfun3(uint32_t ip_)
{
	return (ip_ & 0xffff0000) >> 8 | (ip_ & 0xff);
}

static inline uint32_t hfun4(uint32_t ip_)
{
	return (ip_ & 0xff000000) >> 8 | (ip_ & 0xffff);
}


/*
 * Find the next power of two.
 * from "Hacker's Delight, Henry S. Warren."
 */

static inline
unsigned clp2(unsigned int x)
{
        x = x - 1;
        x = x | (x >> 1);
        x = x | (x >> 2);
        x = x | (x >> 4);
        x = x | (x >> 8);
        x = x | (x >> 16);
        return x + 1;
}


#endif /* _FUNCTIONAL_BLOOM_H_ */
