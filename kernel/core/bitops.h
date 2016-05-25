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

#ifndef Q_CORE_BITOPS_H
#define Q_CORE_BITOPS_H


#define core_ctz(n) \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(n),unsigned int),        (unsigned int)__builtin_ctz((unsigned int)(n)), \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(n),unsigned long),       (unsigned int)__builtin_ctzl((unsigned long)(n)), \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(n),unsigned long long),  (unsigned int)__builtin_ctzll((unsigned long long)(n)), (void)0 )))

#define core_popcount(n) \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(n),unsigned int),        (unsigned int)__builtin_popcount((unsigned int)(n)), \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(n),unsigned long),       (unsigned int)__builtin_popcountl((unsigned long)(n)), \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(n),unsigned long long),  (unsigned int)__builtin_popcountll((unsigned long long)(n)), (void)0)))


#define core_bitwise_foreach(m, n, ...) \
{ \
        typeof(m) mask_ = (m); \
	for(; n = mask_ & -mask_, mask_ ; mask_^=n) \
	__VA_ARGS__ \
}


#endif /* Q_CORE_BITOPS_H */
