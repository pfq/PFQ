/***************************************************************
 *                                                
 * (C) 2011 - Nicola Bonelli <nicola.bonelli@cnit.it>   
 *            Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 *
 ****************************************************************/

#ifndef _SPARSE_COUNTER_H_
#define _SPARSE_COUNTER_H_ 

#include <linux/smp.h> /* get_cpu */

#define MAX_CPU_CTX     64

typedef struct { long value; } __attribute__((aligned(128))) __counter_t; 

typedef struct { __counter_t ctx[MAX_CPU_CTX]; } sparse_counter_t;

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
void sparse_add(long n, sparse_counter_t *sc)
{
    sc->ctx[get_cpu() % MAX_CPU_CTX].value += n;
}

  static inline 
void sparse_sub(long n, sparse_counter_t *sc)
{
    sc->ctx[get_cpu() % MAX_CPU_CTX].value -= n;
}

  static inline
void sparse_set(long n, sparse_counter_t *sc)
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
