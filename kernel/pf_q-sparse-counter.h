/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#ifndef _SPARSE_COUNTER_H_
#define _SPARSE_COUNTER_H_

#include <linux/smp.h> /* get_cpu */

#define MAX_CPU_CTX     64

typedef struct { long value; } __attribute__((aligned(128))) __counter_t;

typedef struct { __counter_t ctx[MAX_CPU_CTX]; } sparse_counter_t;


static inline
void __sparse_inc(sparse_counter_t *sc, int cpu)
{
    sc->ctx[cpu].value++;
}

static inline
void __sparse_dec(sparse_counter_t *sc, int cpu)
{
    sc->ctx[cpu].value--;
}

static inline
void __sparse_add(sparse_counter_t *sc, int cpu, long n)
{
    sc->ctx[cpu].value += n;
}

static inline
void __sparse_sub(sparse_counter_t *sc, int cpu, long n)
{
    sc->ctx[cpu].value -= n;
}


static inline
void sparse_inc(sparse_counter_t *sc)
{
    sc->ctx[get_cpu() % MAX_CPU_CTX].value++;
}

static inline
void sparse_dec(sparse_counter_t *sc)
{
    sc->ctx[get_cpu() % MAX_CPU_CTX].value--;
}

static inline
void sparse_add(sparse_counter_t *sc, long n)
{
    sc->ctx[get_cpu() % MAX_CPU_CTX].value += n;
}

static inline
void sparse_sub(sparse_counter_t *sc, long n)
{
    sc->ctx[get_cpu() % MAX_CPU_CTX].value -= n;
}


static inline
void sparse_set(sparse_counter_t *sc, long n)
{
    unsigned int i, me = get_cpu();
    for(i = 0; i < MAX_CPU_CTX; i++) {
        ((volatile __counter_t *)&sc->ctx[i])->value = (i == me ? n : 0);
    }
}

static inline
long sparse_read(sparse_counter_t *sc)
{
    long ret = 0; int i;
    for(i=0; i < MAX_CPU_CTX; i++) {
        ret += ((volatile __counter_t *)&sc->ctx[i])->value;
    }
    return ret;
}

#endif /* _SPARSE_COUNTER_H_ */
