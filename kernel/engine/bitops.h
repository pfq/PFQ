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

#ifndef Q_ENGINE_BITOPS_H
#define Q_ENGINE_BITOPS_H


#define pfq_ctz(n) \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(n),unsigned int),        (unsigned int)__builtin_ctz(n), \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(n),unsigned long),       (unsigned int)__builtin_ctzl(n), \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(n),unsigned long long),  (unsigned int)__builtin_ctzll(n), (void)0 )))

#define pfq_popcount(n) \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(n),unsigned int),        (unsigned int)__builtin_popcount(n), \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(n),unsigned long),       (unsigned int)__builtin_popcountl(n), \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(n),unsigned long long),  (unsigned int)__builtin_popcountll(n), (void)0)))


#define pfq_bitwise_foreach(m, n, ...) \
{ \
        typeof(m) mask_ = (m); \
	for(; n = mask_ & -mask_, mask_ ; mask_^=n) \
	__VA_ARGS__ \
}


#endif /* Q_ENGINE_BITOPS_H */
