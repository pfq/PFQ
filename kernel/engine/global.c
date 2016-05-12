/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
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

#include <engine/global.h>


int capture_incoming	= 1;
int capture_outgoing	= 0;

int capt_slot_size	= 1514;
int xmit_slot_size	= 1514;

int xmit_batch_len	= 1;
int capt_batch_len	= 1;

int vl_untag		= 0;

int skb_pool_size	= 1024;

int tx_affinity[Q_MAX_CPU] = {0};
int tx_thread_nr;
int tx_rate_control_eager = 1;

