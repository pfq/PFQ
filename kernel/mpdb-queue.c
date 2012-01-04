/***************************************************************
 *                                                
 * (C) 2011-12 Nicola Bonelli <nicola.bonelli@cnit.it>   
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

#include <mpdb-queue.h>

bool 
mpdb_enqueue(struct pfq_opt *pq, struct sk_buff *skb)
{
    struct pfq_queue_descr  *queue_descr = (struct pfq_queue_descr *)pq->q_mem;

    if (!atomic_read((atomic_t *)&queue_descr->disable))  
    {
        size_t packet_len = skb->len + skb->mac_len;
        size_t caplen     = min_t(size_t, pq->q_caplen, packet_len);
        size_t slot_len   = ALIGN(sizeof(struct pfq_hdr) + caplen, 8);

        uint64_t new_data    = atomic64_add_return(0x0000000100000000ULL|slot_len, (atomic64_t *)&queue_descr->data);
        uint64_t queue_size  = DBMP_QUEUE_SIZE(new_data);
        bool     queue_index = DBMP_QUEUE_INDEX(new_data);

        if (queue_size <= pq->q_queue_mem)
        {
            /* enqueue skb */

            struct pfq_hdr * hdr = (struct pfq_hdr *)((char *)(queue_descr+1) + (queue_index ? pq->q_queue_mem : 0) + queue_size - slot_len);
            char * pkt = (char *)(hdr+1);

            /* setup the header */

            hdr->len      = packet_len;
            hdr->caplen   = caplen;
            hdr->if_index = skb->dev->ifindex;
            hdr->hw_queue = skb_get_rx_queue(skb);                      

            if (pq->q_tstamp != 0)
            {
                struct timespec ts;
                skb_get_timestampns(skb, &ts); 
                hdr->tstamp.tv.sec  = ts.tv_sec;
                hdr->tstamp.tv.nsec = ts.tv_nsec;
            }

           /* copy caplen bytes of packet */
           
            if (caplen &&
                skb_copy_bits(skb, /* offset */ -skb->mac_len, pkt, caplen) != 0)
                return false;

           /* commit the slot with release semantic */
           wmb();

           hdr->commit = 1;
           
           /* watermark */

           if ( (queue_size > ( pq->q_queue_mem >> 1)) && 
                           queue_descr->poll_wait ) {
                wake_up_interruptible(&pq->q_waitqueue);
           }

           return true;
        }
        else if ( (queue_size - slot_len) <= pq->q_queue_mem )
        {
            uint64_t valid_data =  new_data - (0x0000000100000000ULL|slot_len);
            
            atomic64_set((atomic64_t *)&queue_descr->valid_data, valid_data);
            
            /* release semantic: volatile variables are not reordered */
            atomic_set((atomic_t *)&queue_descr->disable,1);
        }
    }
    
    if ( queue_descr->poll_wait ) {
        wake_up_interruptible(&pq->q_waitqueue);
    }
    
    return false;
}

