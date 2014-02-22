/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <linux/pf_q.h>

#include <pf_q-common.h>

/* pfq devmap */

enum { map_reset, map_set };

extern atomic_long_t pfq_devmap [Q_MAX_DEVICE][Q_MAX_HW_QUEUE];
extern atomic_t      pfq_devmap_monitor [Q_MAX_DEVICE];


/* called from u-context
*/

extern int  pfq_devmap_update(int action, int index, int queue, int gid);
extern void pfq_devmap_monitor_update(void);

static inline
int __pfq_devmap_equal(int i1, int q1, int i2, int q2)
{
        i1 = i1 > 0 ? i1 & Q_MAX_DEVICE_MASK   : i1;
        q1 = q1 > 0 ? q1 & Q_MAX_HW_QUEUE_MASK : q1;

        i2 = i2 > 0 ? i2 & Q_MAX_DEVICE_MASK   : i2;
        q2 = q2 > 0 ? q2 & Q_MAX_HW_QUEUE_MASK : q2;

        return  (i1 == i2 && q1 == q2) ||
                (i1 == i2 && q2 == -1) ||
                (i2 == -1 && q1 == q2) ||
                (i2 == -1 && q2 == -1);
}


static inline
unsigned long __pfq_devmap_get_groups(int d, int q)
{
        return atomic_long_read(&pfq_devmap[d & Q_MAX_DEVICE_MASK][q & Q_MAX_HW_QUEUE_MASK]);
}


static inline
int __pfq_devmap_monitor_get(int index)
{
        return atomic_read(&pfq_devmap_monitor[index & Q_MAX_DEVICE_MASK]);
}


static inline
void __pfq_devmap_monitor_reset(void)
{
        int n;
        for(n = 0; n < Q_MAX_DEVICE; n++)
        {
                atomic_set(&pfq_devmap_monitor[n],0);
        }
}

#endif /* _PF_Q_DEVMAP_H_ */
