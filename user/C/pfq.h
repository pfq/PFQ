/***************************************************************
   
   Copyright (c) 2012, Nicola Bonelli 
   All rights reserved. 

   Redistribution and use in source and binary forms, with or without 
   modification, are permitted provided that the following conditions are met: 

   * Redistributions of source code must retain the above copyright notice, 
     this list of conditions and the following disclaimer. 
   * Redistributions in binary form must reproduce the above copyright 
     notice, this list of conditions and the following disclaimer in the 
     documentation and/or other materials provided with the distribution. 
   * Neither the name of University of Pisa nor the names of its contributors 
     may be used to endorse or promote products derived from this software 
     without specific prior written permission. 

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
   POSSIBILITY OF SUCH DAMAGE.
 
 ***************************************************************/

#ifndef _PFQ_H_
#define _PFQ_H_ 

#include <stddef.h>
#include <linux/pf_q.h>

/* placeholder type for pfq descriptor */

typedef void * pfq_t;

typedef void (*pfq_handler)(char *user, const struct pfq_hdr *h, const char *data); 

extern pfq_t pfq_default();
extern pfq_t pfq_open(size_t calpen, size_t offset, size_t slots);
extern void  pfq_close(pfq_t *);
extern const char *pfq_error(pfq_t *);

extern void pfq_enable(pfq_t *q, int *ok);
extern int  pfq_disable(pfq_t *q, int *ok);
extern int  pfq_is_enabled(pfq_t const *q, int *ok);

extern void pfq_load_balance(pfq_t *q, int value, int *ok);
extern int pfq_ifindex(pfq_t const *q, const char *dev, int *ok);
extern void pfq_set_tstamp(pfq_t *q, int value, int *ok);
extern int pfq_get_tstamp(pfq_t const *q, int *ok);
extern void pfq_set_caplen(pfq_t *q, size_t value, int *ok);
extern size_t pfq_get_caplen(pfq_t const *q, int *ok);
extern void pfq_set_offset(pfq_t *q, size_t value, int *ok);
extern size_t pfq_get_offset(pfq_t const *q, int *ok);
extern void pfq_set_slots(pfq_t *q, size_t value, int *ok);
extern size_t pfq_get_slots(pfq_t const *q, int *ok);
extern size_t pfq_get_slot_size(pfq_t const *q, int *ok);
extern void pfq_add_device_by_index(pfq_t *q, int index, int queue, int *ok);
extern void pfq_add_device_by_name(pfq_t *q, const char *dev, int queue,int *ok);
extern void pfq_remove_device_by_index(pfq_t *q, int index, int queue, int *ok);
extern void pfq_remove_device_by_name(pfq_t *q, const char *dev, int queue, int *ok);
extern int pfq_poll(pfq_t *q, long int usec, int *ok);
extern int pfq_id(pfq_t const *q, int *ok);
extern int pfq_fd(pfq_t const *q);

extern struct pfq_stats pfq_get_stats(pfq_t const *q, int *ok);

extern int pfq_dispatch(pfq_t *q, pfq_handler callback, char *user, int *ok);

#endif /* _PFQ_H_ */
