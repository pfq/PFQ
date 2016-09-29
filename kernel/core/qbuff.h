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

#ifndef Q_CORE_QBUFF_H
#define Q_CORE_QBUFF_H

#include <core/define.h>
#include <pfq/types.h>


struct pfq_lang_monad;


struct qbuff
{
	void		       *addr;		/* struct sk_buff * */
	struct pfq_lang_monad  *monad;
	struct GC_log	       *log;
        unsigned long		group_mask;
        uint32_t		counter;
        uint32_t		state;
	bool			direct;
};


#define PFQ_DEFINE_QUEUE(name, size) \
	struct name {  \
		size_t len; \
		struct qbuff queue[size]; \
	}


#define PFQ_DEFINE_QUEUE_REF(name, size) \
	struct name {  \
		size_t len; \
		struct qbuff *ref[size]; \
	}


struct core_qbuff_queue
{
	size_t len;
	struct qbuff queue[];
};


struct core_qbuff_refs
{
	size_t len;
	struct qbuff *queue[];
};


PFQ_DEFINE_QUEUE(core_qbuff_batch, Q_CORE_BUFF_BATCH_LEN);
PFQ_DEFINE_QUEUE(core_qbuff_long_queue,  Q_CORE_BUFF_QUEUE_LEN);


PFQ_DEFINE_QUEUE_REF(core_ref_batch, Q_CORE_BUFF_BATCH_LEN);
PFQ_DEFINE_QUEUE_REF(core_ref_long_queue,  Q_CORE_BUFF_QUEUE_LEN);


#define PFQ_QBUFF_QUEUE(q) \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct core_qbuff_batch *),      (struct core_qbuff_queue *)(q), \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct core_qbuff_long_queue *), (struct core_qbuff_queue *)(q), (void)0))


#define PFQ_QBUFF_REFS(q) \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct core_ref_batch *),	    (struct core_qbuff_refs *)(q), \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct core_ref_long_queue *), (struct core_qbuff_refs *)(q), (void)0))


#define PFQ_QBUFF_QUEUE_AT(q, n) \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct core_qbuff_batch *),       (struct qbuff  *)(&((q)->queue[n])), \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct core_qbuff_long_queue *),  (struct qbuff  *)(&((q)->queue[n])), \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct core_qbuff_queue *),       (struct qbuff  *)(&((q)->queue[n])), \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct core_ref_batch *),	      *(struct qbuff **)(&((q)->queue[n])), \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct core_ref_long_queue *),   *(struct qbuff **)(&((q)->queue[n])), \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct core_qbuff_refs *),	      *(struct qbuff **)(&((q)->queue[n])), (void)0))))))


#define STATIC_TYPE(typ, val) __builtin_choose_expr(__builtin_types_compatible_p(typ, typeof((val))), 0, (void)0)


#define for_each_qbuff(q, buff, n) \
        for((n) = 0; ((n) < (q)->len) && ((buff) = PFQ_QBUFF_QUEUE_AT((q),n)); (n)++)


#define for_each_qbuff_with_mask(mask, q, buff, n) \
        for((n) = core_ctz(mask); ((n) < (q)->len) && (mask) && ((buff) = PFQ_QBUFF_QUEUE_AT((q),n)); \
                (mask) ^=(1ULL << (n)), n = core_ctz(mask))


#define for_each_qbuff_from(x, q, buff, n) \
        for((n) = (x); ((n) < (q)->len) && ((buff) = PFQ_QBUFF_QUEUE_AT((q), n)); (n)++)


#define for_each_qbuff_upto(max, q, buff, n) \
        for((n) = 0; ((n) < (max)) && ((n) < (q)->len) && ((buff) = PFQ_QBUFF_QUEUE_AT((q),n)); (n)++)



#endif /* Q_CORE_QBUFF_H */
