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

#include <linux/kernel.h>
#include <linux/version.h>
#include <linux/slab.h>


struct core_spsc_fifo
{
	size_t size;
	size_t tail;
	size_t head;
	void *ring[];
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
bool core_spsc_push(struct core_spsc_fifo *fifo, void *ptr)
{
        size_t w = __atomic_load_n(&fifo->head, __ATOMIC_RELAXED);
        size_t next = core_spsc_next_index(fifo, w);

        if (next == __atomic_load_n(&fifo->tail, __ATOMIC_ACQUIRE))
            return false;

	fifo->ring[w] = ptr;

	__atomic_store_n(&fifo->head, next, __ATOMIC_RELEASE);
        return true;
}



static inline
void *core_spsc_pop(struct core_spsc_fifo *fifo)
{
        size_t w = __atomic_load_n(&fifo->head, __ATOMIC_ACQUIRE);
        size_t r = __atomic_load_n(&fifo->tail, __ATOMIC_RELAXED); // only written from pop thread
        size_t next;
        void *ret;

	if (unlikely(w == r))
		return false;

	ret = fifo->ring[r];

	next = core_spsc_next_index(fifo, r);

	__atomic_store_n(&fifo->tail,next, __ATOMIC_RELEASE);
	return ret;
}


static inline
size_t core_spsc_len(struct core_spsc_fifo const *fifo)
{
	size_t h = __atomic_load_n(&fifo->head, __ATOMIC_ACQUIRE);
	size_t t = __atomic_load_n(&fifo->tail, __ATOMIC_RELAXED);

	if (h >= t)
		return h - t;
	return h + fifo->size - t;
}



static inline
struct core_spsc_fifo *
core_spsc_init(size_t size)
{
	struct core_spsc_fifo *fifo = kmalloc(sizeof(struct core_spsc_fifo) + sizeof(void *)*size, GFP_KERNEL);
	if (fifo != NULL)
	{
		fifo->size = size;
		fifo->head = 0;
		fifo->tail = 0;
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
