/*
 * Copyright (c) 2014 Bonelli Nicola <nicola.bonelli@cnit.it>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met: 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer. 2.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */


#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/version.h>
#include <linux/kthread.h>

#include <pf_q-memory.h>
#include <pf_q-sock.h>
#include <pf_q-transmit.h>
#include <pf_q-common.h>

int
pfq_tx_queue_flush(struct pfq_tx_opt *to, struct net_device *dev)
{
        struct pfq_pkt_hdr * h;
        struct sk_buff *skb;
        int n, index, avail;
        struct local_data *local;
        size_t len;

        index = pfq_spsc_read_index(to->queue_info);
        avail = pfq_spsc_read_avail(to->queue_info);

        local = __this_cpu_ptr(cpu_data);

        for(n = 0; n < avail; n++)
        {
                if (unlikely(index >= to->size))
                {
                        if(printk_ratelimit())
                                printk(KERN_WARNING "[PFQ] bogus spsc index! q->size=%zu index=%d\n", to->size, index);
                        return n;
                }

                h = (struct pfq_pkt_hdr *) (to->base_addr + index * to->queue_info->slot_size);

                skb = pfq_alloc_skb(to->maxlen, GFP_KERNEL);
                if (skb == NULL)
                        return n;

                skb->dev = dev;

                /* copy packet to this skb: */

                len =  min_t(size_t, h->len, to->queue_info->max_len);

                /* set the tail */

                skb_reset_tail_pointer(skb);
                skb->len = 0;
                skb_put(skb, len);


                /* copy the packet in the socket buffer */

                if (skb_store_bits(skb, 0, h+1, len) < 0)
                {
                        pfq_kfree_skb_recycle(skb, &local->recycle_list);
                        return n;
                }

                /* release the slot */

                pfq_spsc_read_commit(to->queue_info);

                /* take this skb */

                skb_get(skb);

                /* send the packet... */
#if 1
                pfq_queue_xmit(skb, to->hw_queue);
#else
                dev_queue_xmit(skb);
#endif
                /* free/recycle the packet now... */

                pfq_kfree_skb_recycle(skb, &local->recycle_list);

                /* get the next index... */

                index = pfq_spsc_read_index(to->queue_info);
                if (index == -1)
                        break;
        }

        return n;
}


int
pfq_tx_thread(void *data)
{
        struct pfq_sock *so = (struct pfq_sock *)data;
        struct net_device *dev;

        printk(KERN_INFO "[PFQ|T] TX thread started on node %d\n", so->tx_opt.cpu_index);

        dev = dev_get_by_index(sock_net(&so->sk), so->tx_opt.if_index);

        for(;;)
        {
                pfq_tx_queue_flush(&so->tx_opt, dev);
                set_current_state(TASK_INTERRUPTIBLE);
                if (kthread_should_stop())
                        break;
                schedule();
        }

        dev_put(dev);

        printk(KERN_INFO "[PFQ|T] TX thread stopped.\n");
        return 0;
}
