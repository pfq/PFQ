/***************************************************************
 *
 * (C) 2014 Nicola Bonelli <nicola@pfq.io>
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

#include <pragma/diagnostic_push>

#include <linux/kernel.h>
#include <linux/printk.h>
#include <linux/pf_q.h>

#include <pragma/diagnostic_pop>

#include <engine/group.h>

#include <pf_q-printk.h>


void
pr_devel_group(pfq_gid_t gid)
{
	struct pfq_group * g;
	g = pfq_get_group(gid);
	if (g != NULL) {

		pr_devel("[PFQ] group %d { policy=%d, pid=%d, owner-id=%d ...}\n",
				gid,
				g->policy,
				g->pid,
				g->owner);
	}
}


void
pr_devel_buffer(const unsigned char *buff, size_t len)
{
	pr_devel("[PFQ] %zu [%2x %2x %2x %2x %2x %2x %2x %2x %2x %2x"
		          " %2x %2x %2x %2x %2x %2x %2x %2x %2x %2x"
		          " %2x %2x %2x %2x %2x %2x %2x %2x %2x %2x"
		          " %2x %2x %2x %2x...]\n", len,
			  buff[0], buff[1], buff[2], buff[3], buff[4], buff[5], buff[6], buff[7],
			  buff[8], buff[9], buff[10], buff[11], buff[12], buff[13], buff[14], buff[15],
			  buff[16], buff[17], buff[18], buff[19], buff[20], buff[21], buff[22], buff[23],
			  buff[24], buff[25], buff[26], buff[27], buff[28], buff[29], buff[30], buff[31],
			  buff[32], buff[33]);
}


