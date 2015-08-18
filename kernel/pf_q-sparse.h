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
#include <linux/smp.h>  /* get_cpu */
#include <asm/local.h>
#include <pragma/diagnostic_pop>

#include <pf_q-define.h>

#ifdef PFQ_USE_EXTENDED_PROC
#define SPARSE_INC(x)	(sparse_inc(x))
#define SPARSE_DEC(x)	(sparse_dec(x))
#define SPARSE_ADD(x,v)	(sparse_add(x,v))
#define SPARSE_SUB(x,v)	(sparse_sub(x,v))
#else
#define SPARSE_INC(x)	((void)x)
#define SPARSE_DEC(x)	((void)x)
#define SPARSE_ADD(x,v)	((void)x)
#define SPARSE_SUB(x,v)	((void)x)
#endif


typedef struct { local_t value; } ____cacheline_aligned counter_t;
typedef struct { counter_t ctx[Q_MAX_CPU]; } sparse_counter_t;


static inline
void __sparse_inc(sparse_counter_t *sc, int cpu)
{
        local_inc(&sc->ctx[cpu & Q_MAX_CPU_MASK].value);
}

static inline
void __sparse_dec(sparse_counter_t *sc, int cpu)
{
        local_dec(&sc->ctx[cpu & Q_MAX_CPU_MASK].value);
}

static inline
void __sparse_add(sparse_counter_t *sc, long n, int cpu)
{
        local_add(n, &sc->ctx[cpu & Q_MAX_CPU_MASK].value);
}

static inline
void __sparse_sub(sparse_counter_t *sc, long n, int cpu)
{
        local_sub(n, &sc->ctx[cpu & Q_MAX_CPU_MASK].value);
}


static inline
void sparse_inc(sparse_counter_t *sc)
{
        __sparse_inc(sc, get_cpu());
        put_cpu();
}


static inline
void sparse_dec(sparse_counter_t *sc)
{
        __sparse_dec(sc, get_cpu());
        put_cpu();
}

static inline
void sparse_add(sparse_counter_t *sc, long n)
{
        __sparse_add(sc, n, get_cpu());
        put_cpu();
}

static inline
void sparse_sub(sparse_counter_t *sc, long n)
{
        __sparse_sub(sc, n, get_cpu());
        put_cpu();
}


static inline
void sparse_set(sparse_counter_t *sc, long n)
{
        unsigned int i, me = get_cpu();
        for(i = 0; i < Q_MAX_CPU; i++)
                local_set(&sc->ctx[i].value, i == me ? n : 0);
        put_cpu();
}

static inline
long sparse_read(sparse_counter_t *sc)
{
        long ret = 0; int i;
        for(i=0; i < Q_MAX_CPU; i++)
                ret += local_read(&sc->ctx[i].value);

        return ret;
}

#endif /* PF_Q_SPARSE_H */
