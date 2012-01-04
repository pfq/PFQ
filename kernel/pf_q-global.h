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

#ifndef _PF_Q_GLOBAL_H_
#define _PF_Q_GLOBAL_H_ 

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/semaphore.h>

#define __PFQ_MODULE__
#include <linux/pf_q.h>

struct pfq_global_t
{
    /* devmap */
    volatile unsigned long devmap  [Q_MAX_DEVICE][Q_MAX_HW_QUEUE];
    volatile uint8_t devmap_monitor[Q_MAX_DEVICE];

    /* timestamp */
    atomic_t   tstamp;

};

extern struct pfq_global_t global;
extern struct semaphore    global_sem;

#endif /* _PF_Q_GLOBAL_H_ */
