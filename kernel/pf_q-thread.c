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
pfq_tx_thread(void *data)
{
        struct pfq_sock *so = (struct pfq_sock *)data;
        struct net_device *dev;
	int cpu, node;

        dev = dev_get_by_index(sock_net(&so->sk), so->tx_opt.if_index);

        cpu  = smp_processor_id();
	node = cpu_to_node(cpu);

        printk(KERN_INFO "[PFQ|T] TX thread started on cpu %d:%d...\n", so->tx_opt.cpu, node);

        for(;;)
        {
                pfq_tx_queue_flush(&so->tx_opt, dev, cpu, node);
                set_current_state(TASK_INTERRUPTIBLE);

                if (kthread_should_stop())
                        break;
                schedule();
        }

        dev_put(dev);

        printk(KERN_INFO "[PFQ|T] TX thread stopped on cpu %d:%d.\n", so->tx_opt.cpu, node);
        return 0;
}
