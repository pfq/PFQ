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

#ifndef _PF_Q_DEVMAP_H_
#define _PF_Q_DEVMAP_H_ 

#define __PFQ_MODULE__
#include <linux/pf_q.h>

#include <pf_q-priv.h>
#include <pf_q-global.h>

/* pfq devmap */

enum { map_reset, map_set };


// called from u-context
//

extern
int pfq_devmap_update(int action, int index, int queue, unsigned int id);

extern
void pfq_devmap_monitor_update(void);
  

static inline 
int pfq_devmap_equal(int i1, int q1, int i2, int q2)
{
    i1 = i1 > 0 ? i1 & Q_MAX_DEVICE_MASK : i1;
    i2 = i2 > 0 ? i2 & Q_MAX_DEVICE_MASK : i2;
    
    q1 = q1 > 0 ? q1 & Q_MAX_HW_QUEUE_MASK : q1;
    q2 = q2 > 0 ? q2 & Q_MAX_HW_QUEUE_MASK : q2;

    return  (i1 == i2 && q1 == q2) ||
            (i1 == i2 && q2 == -1) ||
            (i2 == -1 && q1 == q2) ||
            (i2 == -1 && q2 == -1);
}


static inline 
unsigned long pfq_devmap_get(int d, int q)
{
    return global.devmap[d & Q_MAX_DEVICE_MASK][q & Q_MAX_HW_QUEUE_MASK];
}


static inline 
uint8_t pfq_devmap_monitor_get(int index)
{
    return global.devmap_monitor[index & Q_MAX_DEVICE_MASK];
}


#endif /* _PF_Q_DEVMAP_H_ */
