/***************************************************************
 *
 * (C) 2014 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <linux/printk.h>

#include <linux/pf_q.h>
#include <linux/pf_q-fun.h>

#include <pf_q-functional.h>

struct pfq_exec_prog * pfq_fun_prog_compile(const struct pfq_fun_prog *source)
{
        return NULL;
}

void pfq_fun_prog_print(const struct pfq_fun_prog *source)
{
        int n;
        for(n = 0; n < source->size; n++)
        {
                pr_devel("    %d: %s (%p,%d)\n", n, source->fun[n].name, source->fun[n].context.addr, source->fun[n].context.size);
        }
}

void pfq_exec_prog_print(const struct pfq_exec_prog *source)
{
        int n;
        for(n = 0; n < source->size; n++)
        {
                pr_devel("    %d: f:%p c:%p\n", n, source->step[n].ptr_fun, source->step[n].ptr_ctx);
        }
}
