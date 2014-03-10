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

#ifndef _PF_Q_FUNCTIONAL_H_
#define _PF_Q_FUNCTIONAL_H_

#include <linux/pf_q.h>
#include <linux/pf_q-fun.h>


typedef struct pfq_exec
{
        void *  ptr_fun;
        void *  ptr_ctx;
        long    lock_ctx;

} pfq_exec_t;


struct pfq_exec_prog
{
        int size;
        pfq_exec_t step[];
};

extern struct pfq_exec_prog * pfq_fun_prog_compile(const struct pfq_fun_prog *source);

extern void   pfq_fun_prog_print(const struct pfq_fun_prog *source);
extern void   pfq_exec_prog_print(const struct pfq_exec_prog *source);


#endif /* _PF_Q_FUNCTIONAL_H_ */
