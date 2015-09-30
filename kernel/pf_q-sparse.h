/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PF_Q_SPARSE_H
#define PF_Q_SPARSE_H

#include <pragma/diagnostic_push>
#include <linux/smp.h>
#include <linux/percpu.h>
#include <asm/local.h>
#include <pragma/diagnostic_pop>


/*
 * a group of sparse counters is a per-cpu generic structure of local_t members
 */

#define sparse_read(ptr, var) ({ \
	long ret = 0; int i; \
	for_each_possible_cpu(i) { \
		ret += local_read(&(per_cpu_ptr(ptr, i)->var)); \
	} \
	ret; \
})


#define sparse_set(ptr, var, value) ({ \
	long ret = 0; int i, this = get_cpu(); \
	for_each_possible_cpu(i) { \
		local_set(&(per_cpu_ptr(ptr, i)->var)), i == this ? value : 0); \
	} \
	put_cpu(); \
})


#define sparse_add(ptr, var, value) ({ \
        local_add(value, &(this_cpu_ptr(ptr)->var)); \
})


#define sparse_sub(ptr, var, value) ({ \
        local_sub(value, &(this_cpu_ptr(ptr)->var)); \
})


#define sparse_inc(ptr, var) ({ \
        local_inc(&(this_cpu_ptr(ptr)->var)); \
})


#define sparse_dec(ptr, var) ({ \
        local_dec(&(this_cpu_ptr(ptr)->var)); \
})


#define __sparse_add(ptr, var, value, cpu) ({ \
        local_add(value, &(per_cpu_ptr(ptr, cpu)->var)); \
})


#define __sparse_sub(ptr, var, value, cpu) ({ \
        local_sub(value, &(per_cpu_ptr(ptr, cpu)->var)); \
})


#define __sparse_inc(ptr, var, cpu) ({ \
        local_inc(&(per_cpu_ptr(ptr, cpu)->var)); \
})


#define __sparse_dec(ptr, var, cpu) ({ \
        local_dec(&per_cpu_ptr(ptr, cpu)->var); \
})


#endif /* PF_Q_SPARSE_H */
