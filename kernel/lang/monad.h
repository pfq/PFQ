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

#ifndef PFQ_LANG_MONAD_H
#define PFQ_LANG_MONAD_H

#include <pfq/group.h>
#include <pfq/define.h>
#include <pfq/GC.h>

#include <pfq/sparse.h>
#include <pfq/kcompat.h>
#include <pfq/qbuff.h>

/* The Action monad */

typedef struct
{
	struct qbuff *	qbuff;

} ActionQbuff;


/* fanout: actions types */

enum fanout
{
        fanout_drop   = 0,
        fanout_copy   = 1,
        fanout_steer  = 2,
        fanout_double = 3
};


typedef struct
{
        unsigned long	class_mask;
        uint32_t	hash;
        uint32_t	hash2;
        uint8_t		type;

} fanout_t;

/* endpoint context */

#define EPOINT_SRC	(1<<0)
#define EPOINT_DST	(1<<1)

/* Action monad */

struct pfq_lang_monad
{
        struct pfq_group	*group;
        uint32_t		state;
        fanout_t		fanout;
        int			shift;
	int			ipoff;
        int			ipproto;
        int			ep_ctx;		/* endpoint context */
};

/* Fanout constructors */

static inline
ActionQbuff
Pass(struct qbuff * buff)
{
        return (ActionQbuff){buff};
}

static inline
ActionQbuff
Drop(struct qbuff * buff)
{
        buff->monad->fanout.type = fanout_drop;
        return (ActionQbuff){buff};
}

static inline
ActionQbuff
Copy(struct qbuff * buff)
{
        buff->monad->fanout.type = fanout_copy;
        return (ActionQbuff){buff};
}

static inline
ActionQbuff
Broadcast(struct qbuff * buff)
{
        fanout_t * a  = &buff->monad->fanout;
        a->class_mask = Q_CLASS_ANY;
        a->type       = fanout_copy;
        return (ActionQbuff){buff};
}

static inline
ActionQbuff
Steering(struct qbuff * buff, uint32_t hash)
{
        fanout_t * a = &buff->monad->fanout;
        a->type  = fanout_steer;
        a->hash  = hash;
        return (ActionQbuff){buff};
}

static inline
ActionQbuff
DoubleSteering(struct qbuff * buff, uint32_t h1, uint32_t h2)
{
        fanout_t * a = &buff->monad->fanout;
        a->type  = fanout_double;
        a->hash  = h1;
        a->hash2 = h2;
        return (ActionQbuff){buff};
}

/* utility functions */

static inline
struct qbuff *
class(struct qbuff * buff, uint64_t class_mask)
{
        buff->monad->fanout.class_mask = class_mask;
        return buff;
}

static inline
struct qbuff *
to_kernel(struct qbuff * buff)
{
        buff->log->to_kernel = true;
        return buff;
}


static inline
uint32_t get_mark(struct qbuff * buff)
{
        return qbuff_get_mark(buff);
}

static inline
void set_mark(struct qbuff * buff, uint32_t value)
{
        qbuff_set_mark(buff,value);
}

static inline
uint32_t get_state(struct qbuff * buff)
{
        return buff->monad->state;
}

static inline
void set_state(struct qbuff * buff, uint32_t state)
{
        buff->monad->state = state;
}

static inline
pfq_group_stats_t * get_group_stats(struct qbuff * buff)
{
	return this_cpu_ptr(buff->monad->group->stats);
}

static inline
struct pfq_group_counters * get_group_counters(struct qbuff * buff)
{
	return this_cpu_ptr(buff->monad->group->counters);
}


#endif /* PFQ_LANG_MONAD_H */
