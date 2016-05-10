/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
 *             Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>
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

#ifndef Q_ENGINE_CORE_H
#define Q_ENGINE_CORE_H

#include <engine/lang/engine.h>
#include <engine/lang/GC.h>

#include <engine/percpu.h>


int pfq_process_batch(struct pfq_percpu_data *data,
		      struct pfq_percpu_sock *sock,
		      struct pfq_percpu_pool *pool,
		      struct GC_data *GC_ptr,
		      int cpu);


#endif /* Q_ENGINE_CORE_H */

