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

#ifndef PFQ_LANG_MAYBE_H
#define PFQ_LANG_MAYBE_H

#define NOTHING			~0ULL
#define FROM_JUST(type, a)	((long long int)a < 0 ? (type)(a+1) : (type)a)

#define JUST(a) \
	__builtin_choose_expr(__builtin_types_compatible_p(char,	           typeof(a)),  _JUST(a), \
	__builtin_choose_expr(__builtin_types_compatible_p(short int,		   typeof(a)),  _JUST(a), \
	__builtin_choose_expr(__builtin_types_compatible_p(int,			   typeof(a)),  _JUST(a), \
	__builtin_choose_expr(__builtin_types_compatible_p(long int,		   typeof(a)),  _JUST(a), \
	__builtin_choose_expr(__builtin_types_compatible_p(long long int,	   typeof(a)),  _JUST(a), \
	__builtin_choose_expr(__builtin_types_compatible_p(unsigned char,	   typeof(a)),  ((long long int)a), \
	__builtin_choose_expr(__builtin_types_compatible_p(unsigned short int,	   typeof(a)),  ((long long int)a), \
	__builtin_choose_expr(__builtin_types_compatible_p(unsigned int,	   typeof(a)),  ((long long int)a), \
	__builtin_choose_expr(__builtin_types_compatible_p(unsigned long int,	   typeof(a)),  ((long long int)a), \
	__builtin_choose_expr(__builtin_types_compatible_p(unsigned long long int, typeof(a)),  _JUST(a), \
	(void)0))))))))))


#define _JUST(a) ({ long long int _a = (long long int)(a); (_a < 0 ? _a-1: _a); })


#define IS_JUST(a) \
	__builtin_choose_expr(__builtin_types_compatible_p(long long int, typeof(a)),		((a) != (typeof(a))~0LL),\
	__builtin_choose_expr(__builtin_types_compatible_p(unsigned long long int, typeof(a)),  ((a) != (typeof(a))~0LL),\
	__builtin_choose_expr(__builtin_types_compatible_p(int64_t, typeof(a)),			((a) != (typeof(a))~0LL),\
	__builtin_choose_expr(__builtin_types_compatible_p(uint64_t, typeof(a)),		((a) != (typeof(a))~0LL),\
	(void)0))))


#define IS_NOTHING(a) \
	__builtin_choose_expr(__builtin_types_compatible_p(long long int, typeof(a)),		((a) == (typeof(a))~0LL),\
	__builtin_choose_expr(__builtin_types_compatible_p(unsigned long long int, typeof(a)),  ((a) == (typeof(a))~0LL),\
	__builtin_choose_expr(__builtin_types_compatible_p(int64_t, typeof(a)),			((a) == (typeof(a))~0LL),\
	__builtin_choose_expr(__builtin_types_compatible_p(uint64_t, typeof(a)),		((a) == (typeof(a))~0LL),\
	(void)0))))


#endif /* PFQ_LANG_MAYBE_H */
