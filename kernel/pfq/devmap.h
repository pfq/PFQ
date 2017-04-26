/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PFQ_DEVMAP_H
#define PFQ_DEVMAP_H

#include <pfq/global.h>
#include <pfq/group.h>
#include <pfq/define.h>
#include <pfq/kcompat.h>


/* pfq devmap */

enum
{      Q_DEVMAP_RESET,
       Q_DEVMAP_SET
};


/* called from u-context
*/

extern int  pfq_devmap_update(int action, int index, int queue, pfq_gid_t gid);


static inline
int pfq_devmap_equal(int i1, int q1, int i2, int q2)
{
        return  (i1 == i2 && q1 == q2) ||
                (i1 == i2 && q2 == -1) ||
                (i2 == -1 && q1 == q2) ||
                (i2 == -1 && q2 == -1);
}


static inline
unsigned long pfq_devmap_get_groups(int dev, int queue)
{
        return (long unsigned)atomic_long_read(&global->devmap[dev & Q_MAX_DEVICE_MASK][queue & Q_MAX_QUEUE_MASK]);
}


extern void pfq_devmap_toggle_update(void);

static inline
int pfq_devmap_toggle_get(int index)
{
        return atomic_read(&global->devmap_toggle[index & Q_MAX_DEVICE_MASK]);
}


static inline
void pfq_devmap_toggle_reset(void)
{
        int n;
        for(n = 0; n < Q_MAX_DEVICE; n++)
        {
                atomic_set(&global->devmap_toggle[n],0);
        }
}

#endif /* PFQ_DEVMAP_H */
