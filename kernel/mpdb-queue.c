/***************************************************************
 *                                                
 * (C) 2011 - Nicola Bonelli <nicola.bonelli@cnit.it>   
 *            Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 *
 ****************************************************************/

#include <mpdb-queue.h>

void *
mpdb_queue_alloc(struct pfq_opt *pq, int queue_mem, size_t * tot_mem)
{
        /* calculate the size of the buffer */

        int tm = PAGE_ALIGN(queue_mem); 

        /* align bufflen to page size */

        int num_pages = tm / PAGE_SIZE; void *addr;

        num_pages += (num_pages + (SHMLBA-1)) % SHMLBA;
        *tot_mem = num_pages*PAGE_SIZE;

        /* Memory is already zeroed */
        addr = vmalloc_user(*tot_mem);
        if (addr == NULL)
        {
                printk(KERN_INFO "[PF_Q] pfq_queue_alloc: out of memory");
                *tot_mem = 0;
                return NULL;
        }

        printk(KERN_INFO "[PF_Q] queue caplen:%lu mem:%lu\n", pq->q_caplen, *tot_mem); 
        return addr;
}


void
mpdb_queue_free(struct pfq_opt *pq)
{
        if (pq->q_addr) {
                printk(KERN_INFO "[PF_Q] queue freed!\n"); 
                vfree(pq->q_addr);

                pq->q_addr = NULL;
                pq->q_queue_mem = 0;
        }
}    


bool 
mpdb_enqueue(struct pfq_opt *pq, struct sk_buff *skb)
{
        struct pfq_queue_descr  *queue_descr = (struct pfq_queue_descr *)pq->q_addr;

        if (!atomic_read((atomic_t *)&queue_descr->disabled))  
        {
                size_t packet_len= skb->len + skb->mac_len;
                size_t cap_len   = min(packet_len, pq->q_caplen);

                int  data    = atomic_add_return(1, (atomic_t *)&queue_descr->data);
                int  q_len   = DBMP_QUEUE_LEN(data);
                bool q_index = DBMP_QUEUE_INDEX(data);

                if (q_len <= pq->q_slots)
                {
                        /* enqueue skb */

                        struct pfq_hdr *p_hdr = (struct pfq_hdr *)((char *)(queue_descr+1) + q_index * pq->q_slot_size * pq->q_slots 
                                        + (q_len-1) * pq->q_slot_size);

                        char *p_pkt = (char *)(p_hdr+1);

                        /* copy caplen bytes of packet */

                        if (pq->q_caplen &&
                                skb_copy_bits(skb, /* offset */ -skb->mac_len, p_pkt, cap_len) != 0)
                        {    
                                return false;
                        }

                        /* setup the header */

                        p_hdr->len      = packet_len;
                        p_hdr->caplen   = cap_len;
                        p_hdr->if_index = skb->dev->ifindex;
                        p_hdr->hw_queue = skb_get_rx_queue(skb);                      

                        if (pq->q_tstamp != 0)
                        {
                                struct timespec ts;
                                skb_get_timestampns(skb, &ts); 
                                p_hdr->tstamp.tv.sec  = ts.tv_sec;
                                p_hdr->tstamp.tv.nsec = ts.tv_nsec;
                        }

                        /* commit the slot with release semantic */
                        wmb();

                        p_hdr->commit = 1;

                        /* watermark */

                        if ((q_len > ( pq->q_slots >> 1)) 
                                        && queue_descr->poll_wait ) {
                                wake_up_interruptible(&pq->q_waitqueue);
                        }

                        return true;
                }
                else if (q_len == (pq->q_slots+1))
                {
                        atomic_set((atomic_t *)&queue_descr->disabled,1);
                }
        }

        if ( queue_descr->poll_wait ) {
                wake_up_interruptible(&pq->q_waitqueue);
        }

        return false;
}

