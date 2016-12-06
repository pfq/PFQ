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

#ifndef Q_CORE_SPSC_FIFO_H
#define Q_CORE_SPSC_FIFO_H

#ifdef __KERNEL__
#include <linux/slab.h>
#else
#include <stdlib.h>
#define likely(x)		__builtin_expect((x),1)
#define unlikely(x)		__builtin_expect((x),0)
#define kmalloc(s,m)		malloc(s)
#define kmalloc_node(s,m,n)	malloc(s)
#define kfree(s) free(s)
#endif


#define SPSC_FIFO_BATCH		64


struct core_spsc_fifo
{
	struct
	{
		size_t tail_cache;

	} __attribute__((aligned(128)));

	struct
	{
		size_t head_cache;

	} __attribute__((aligned(128)));

	struct
	{
		size_t size;
		size_t tail;
		size_t head;
		void *ring[];
	} __attribute__((aligned(128)));
};


static inline
size_t core_spsc_next_index(struct core_spsc_fifo *fifo, size_t value)
{
	size_t ret = value + 1;
	while (unlikely(ret >= fifo->size)) {
		ret -= fifo->size;
	}
	return ret;
}


static inline
bool core_spsc_is_empty(struct core_spsc_fifo const *fifo)
{
	return __atomic_load_n(&fifo->head, __ATOMIC_ACQUIRE) ==
	       __atomic_load_n(&fifo->tail, __ATOMIC_RELAXED);
}



static inline
bool core_spsc_is_full(struct core_spsc_fifo const *fifo)
{
	return (__atomic_load_n(&fifo->tail, __ATOMIC_ACQUIRE) + 1) % (fifo->size) ==
		__atomic_load_n(&fifo->head, __ATOMIC_RELAXED);
}


static inline
size_t core_spsc_distance(struct core_spsc_fifo const *fifo, size_t h, size_t t)
{
	if (likely(h >= t))
		return h - t;
	return h + fifo->size - t;
}


static inline
size_t core_spsc_len(struct core_spsc_fifo const *fifo)
{
	size_t h = __atomic_load_n(&fifo->head_cache, __ATOMIC_ACQUIRE);
	size_t t = __atomic_load_n(&fifo->tail_cache, __ATOMIC_RELAXED);
	return core_spsc_distance(fifo, h, t);
}


static inline
void core_spsc_push_sync(struct core_spsc_fifo *fifo)
{
	__atomic_store_n(&fifo->head, fifo->head_cache, __ATOMIC_RELEASE);
}


static inline
void core_spsc_pop_sync(struct core_spsc_fifo *fifo)
{
	__atomic_store_n(&fifo->tail, fifo->tail_cache, __ATOMIC_RELEASE);
}


static inline
bool core_spsc_push(struct core_spsc_fifo *fifo, void *ptr)
{
        size_t w = fifo->head_cache;
        size_t next = core_spsc_next_index(fifo, w);

        if (unlikely(next == __atomic_load_n(&fifo->tail, __ATOMIC_ACQUIRE))) {
	    core_spsc_push_sync(fifo);
            return false;
	}

	fifo->ring[w] = ptr;
	fifo->head_cache = next;

	if (core_spsc_distance(fifo, fifo->head_cache, __atomic_load_n(&fifo->head, __ATOMIC_RELAXED))
	    > SPSC_FIFO_BATCH) {
		core_spsc_push_sync(fifo);
	}

        return true;
}


static inline
void *core_spsc_pop(struct core_spsc_fifo *fifo)
{
        size_t w = __atomic_load_n(&fifo->head, __ATOMIC_ACQUIRE);
        size_t r = fifo->tail_cache;
        size_t next;
        void *ret;

	if (unlikely(w == r)) {
		core_spsc_pop_sync(fifo);
		return NULL;
	}

	ret = fifo->ring[r];
	next = core_spsc_next_index(fifo, r);

	fifo->tail_cache = next;

	if (core_spsc_distance(fifo, fifo->tail_cache, __atomic_load_n(&fifo->tail, __ATOMIC_RELAXED))
	    > SPSC_FIFO_BATCH) {
		core_spsc_pop_sync(fifo);
	}

	return ret;
}


static inline
void *core_spsc_peek(struct core_spsc_fifo *fifo)
{
        size_t w = __atomic_load_n(&fifo->head, __ATOMIC_ACQUIRE);
        size_t r = fifo->tail_cache;

	if (unlikely(w == r)) {
		core_spsc_pop_sync(fifo);
		return NULL;
	}

	return fifo->ring[r];
}


static inline
void core_spsc_consume(struct core_spsc_fifo *fifo)
{
	size_t next = core_spsc_next_index(fifo, fifo->tail_cache);

	fifo->tail_cache = next;
	if (core_spsc_distance(fifo, fifo->tail_cache, __atomic_load_n(&fifo->tail, __ATOMIC_RELAXED))
	    > SPSC_FIFO_BATCH) {
		core_spsc_pop_sync(fifo);
	}
}



static inline
struct core_spsc_fifo *
core_spsc_init(size_t size, int cpu)
{
	struct core_spsc_fifo *fifo = (struct core_spsc_fifo *)
		kmalloc_node(sizeof(struct core_spsc_fifo) + sizeof(void *)*(size+1), GFP_KERNEL, cpu_to_node(cpu));
	if (fifo != NULL)
	{
		fifo->size = size+1;
		fifo->head = 0;
		fifo->tail = 0;
		fifo->head_cache = 0;
		fifo->tail_cache = 0;
	}
	return fifo;
}


static inline
void core_spsc_free(struct core_spsc_fifo *fifo, void (*free_)(void *))
{
	void *ptr;
	while ((ptr = core_spsc_pop(fifo)))
		free_(ptr);
	kfree(fifo);
}


#endif /* Q_CORE_SPSC_FIFO_H_ */
