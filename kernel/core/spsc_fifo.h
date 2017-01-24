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
#include <linux/pf_q.h>


#define SPSC_FIFO_BATCH		64


struct core_spsc_fifo
{
	struct
	{
		size_t head_cache;

	} consumer ____pfq_cacheline_aligned;

	struct
	{
		size_t tail_cache;

	} producer ____pfq_cacheline_aligned;

	struct
	{
		size_t head;
	} ____pfq_cacheline_aligned;

	struct
	{
		size_t tail;
	} ____pfq_cacheline_aligned;

	struct
	{
		size_t size;
		void *ring[];

	} ____pfq_cacheline_aligned;
};



static inline
void core_spsc_dump(const char *msg, struct core_spsc_fifo const *fifo)
{
#ifdef __KERNEL__
	printk(KERN_INFO "[PFQ] %s[%zu] SPSC { head:%zu tail:%zu producer:{ tail:%zu } consumer:{ head:%zu }}\n"
	     , msg
	     , fifo->size
	     , __atomic_load_n(&fifo->head, __ATOMIC_RELAXED)
	     , __atomic_load_n(&fifo->tail, __ATOMIC_RELAXED)
	     , __atomic_load_n(&fifo->producer.tail_cache, __ATOMIC_RELAXED)
	     , __atomic_load_n(&fifo->consumer.head_cache, __ATOMIC_RELAXED)
	     );
#endif
}


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
	if (h >= t)
		return h - t;
	return h + fifo->size - t;
}


static inline
void core_spsc_consumer_sync(struct core_spsc_fifo *fifo)
{
	fifo->consumer.head_cache = __atomic_load_n(&fifo->head, __ATOMIC_ACQUIRE);
}


static inline
void core_spsc_producer_sync(struct core_spsc_fifo *fifo)
{
	fifo->producer.tail_cache = __atomic_load_n(&fifo->tail, __ATOMIC_ACQUIRE);
}


static inline
size_t core_spsc_len(struct core_spsc_fifo *fifo)
{
	size_t h, t;
	h = __atomic_load_n(&fifo->head, __ATOMIC_RELAXED);
	t = __atomic_load_n(&fifo->tail, __ATOMIC_RELAXED);
	return core_spsc_distance(fifo, h, t);
}



static inline
size_t core_spsc_push(struct core_spsc_fifo *fifo, void *ptr)
{
        size_t w = __atomic_load_n(&fifo->head, __ATOMIC_RELAXED);
        size_t r = fifo->producer.tail_cache;

        size_t next = core_spsc_next_index(fifo, w);

	if (next == r) {
		r = fifo->producer.tail_cache = __atomic_load_n(&fifo->tail, __ATOMIC_ACQUIRE);
		if (next == r) {
			return 0;
		}
	}

	fifo->ring[w] = ptr;
	__atomic_store_n(&fifo->head, next, __ATOMIC_RELEASE);
        return core_spsc_distance(fifo, next, r);
}


static inline
void *core_spsc_pop(struct core_spsc_fifo *fifo)
{
        size_t w = fifo->consumer.head_cache;
        size_t r = __atomic_load_n(&fifo->tail, __ATOMIC_RELAXED);
        size_t next;
        void *ret;

	if (w == r) {
		w = fifo->consumer.head_cache = __atomic_load_n(&fifo->head, __ATOMIC_ACQUIRE);
		if (w == r)
			return NULL;
	}

	ret = fifo->ring[r];
	next = core_spsc_next_index(fifo, r);
        __atomic_store_n(&fifo->tail, next, __ATOMIC_RELEASE);
	return ret;
}


static inline
void *core_spsc_peek(struct core_spsc_fifo *fifo)
{
        size_t w = fifo->consumer.head_cache;
        size_t r = __atomic_load_n(&fifo->tail, __ATOMIC_RELAXED);

	if (w == r) {
		w = fifo->consumer.head_cache = __atomic_load_n(&fifo->head, __ATOMIC_ACQUIRE);
		if (w == r)
			return NULL;
	}

	return fifo->ring[r];
}


static inline
void core_spsc_consume(struct core_spsc_fifo *fifo)
{
	size_t next = core_spsc_next_index(fifo, fifo->tail);
	__atomic_store_n(&fifo->tail, next, __ATOMIC_RELEASE);
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
		fifo->producer.tail_cache = 0;
		fifo->consumer.head_cache = 0;
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
