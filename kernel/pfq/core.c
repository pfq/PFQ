/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
 *             Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 * 	       Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>
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

#include <lang/engine.h>
#include <lang/symtable.h>

#include <pfq/bitops.h>
#include <pfq/core.h>
#include <pfq/devmap.h>
#include <pfq/global.h>
#include <pfq/io.h>
#include <pfq/kcompat.h>
#include <pfq/memory.h>
#include <pfq/percpu.h>
#include <pfq/prefetch.h>
#include <pfq/qbuff.h>
#include <pfq/queue.h>
#include <pfq/sock.h>
#include <pfq/vlan.h>



int pfq_receive_run( struct pfq_percpu_data *data
		   , struct pfq_percpu_pool *pool
		   , struct qbuff **ptr
		   , size_t len
		   , int cpu)
{
#if 0
	unsigned long long sock_queue[Q_BUFF_BATCH_LEN] = { 0 };
        unsigned long all_group_mask, socket_mask;
	struct pfq_endpoint_info endpoints;
	struct pfq_lang_monad monad;
        long unsigned n, bit, lb;
	size_t current_batch_len;
        struct qbuff *buff;


	/* copy payloads to endpoints... */

	pfq_bitwise_foreach(socket_mask, lb,
			    {
			    pfq_id_t id = (__force pfq_id_t)pfq_ctz(lb);
			    struct pfq_sock * so = pfq_sock_get_by_id(id);
			    pfq_copy_to_endpoint_qbuffs(so, PFQ_QBUFF_REFS(&refs), sock_queue[(int __force)id], cpu, gid);
		})

	/* forward buffs to network devices */

	// TODO
	//GC_get_lazy_endpoints(GC_ptr, &endpoints);
	//if (endpoints.cnt_total)
	//{
	//	size_t total = (size_t)pfq_qbuff_lazy_xmit_run(PFQ_QBUFF_QUEUE(&GC_ptr->pool), &endpoints);
	//	__sparse_add(global->percpu_stats, frwd, total, cpu);
	//	__sparse_add(global->percpu_stats, disc, endpoints.cnt_total - total, cpu);
	//}


	/* forward buffs to kernel and release them... */

	// TODO
	// for_each_qbuff(PFQ_QBUFF_QUEUE(&GC_ptr->pool), buff, n)
	{
		if (fwd_to_kernel(buff)) {

			bool peeked = QBUFF_SKB(buff)->peeked;

			qbuff_move_or_copy_to_kernel(buff, GFP_KERNEL);

			/* only if peeked we need to free/recycle the qbuff/skb */
			if (peeked)
				qbuff_free(buff, &pool->rx);

			__sparse_inc(global->percpu_stats, kern, cpu);
		}
		else {
			/* Peeked or not, always free the qbuff/skb here */
			qbuff_free(buff, &pool->rx);
		}
	}

	/* reset the GC */

	// TODO
	// GC_reset(GC_ptr);
        return 0;

#endif
}

