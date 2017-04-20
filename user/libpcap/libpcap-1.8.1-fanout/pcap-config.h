/*
 * Copyright (c) 1994, 1995, 1996
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the Computer Systems
 *	Engineering Group at Lawrence Berkeley Laboratory.
 * 4. Neither the name of the University nor of the Laboratory may be used
 *    to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef pcap_config_h
#define	pcap_config_h

#include <stddef.h>
#include <pcap-int.h>

#include "config.h"

#define PCAP_CONF_KEY(value) [PCAP_CONF_KEY_ ## value] = # value

#define PCAP_CONF_KEY_error	       -1
#define PCAP_CONF_KEY_def_group		0
#define PCAP_CONF_KEY_fanout		1
#define PCAP_CONF_KEY_caplen		2

#ifdef PCAP_SUPPORT_PFQ
/* specific PFQ keys */

#define PCAP_CONF_KEY_pfq_rx_slots	3
#define PCAP_CONF_KEY_pfq_tx_slots	4
#define PCAP_CONF_KEY_pfq_tx_sync	5
#define PCAP_CONF_KEY_pfq_tx_hw_queue	6
#define PCAP_CONF_KEY_pfq_tx_idx_thread	7
#define PCAP_CONF_KEY_pfq_vlan		8
#endif



#define PCAP_FANOUT_GROUP_MAX		64
#define PCAP_FANOUT_GROUP_DEFAULT	64

struct pcap_conf_key
{
	const char *value;
};

extern struct pcap_conf_key pcap_conf_keys[];


struct pcap_group_map
{
	struct {
		char	*dev;
		int	group;
	} entry[PCAP_FANOUT_GROUP_MAX];

	int size;
};


struct pcap_config
{
	int    def_group;
	struct pcap_group_map group_map;
	char   *fanout[PCAP_FANOUT_GROUP_DEFAULT+1];
	int    caplen;

#ifdef PCAP_SUPPORT_PFQ


	int pfq_rx_slots;
	int pfq_tx_slots;

	int pfq_tx_sync;
	int pfq_tx_async;

	int pfq_tx_hw_queue[4];
	int pfq_tx_idx_thread[4];

	char *pfq_vlan [PCAP_FANOUT_GROUP_DEFAULT+1];
#endif

};


typedef int (*pcap_string_handler_t)(const char *);

struct pcap_config pcap_config_default(pcap_t *handle);

int    pcap_parse_config(struct pcap_config *opt, const char *filename);

int    pcap_string_for_each_token(const char *ds, const char *sep, pcap_string_handler_t handler);
char * pcap_string_first_token(const char *str, const char *sep);
char * pcap_string_trim(char *str);
char * pcap_string_append(char *str1, const char *str2);

int    pcap_parse_integers(int *out, size_t max, const char *in);
char * pcap_getenv_name(char const *var);
char * pcap_getenv_value(char const *var);
char **pcap_getenv(char const *var);

void pcap_group_map_dump(struct pcap_group_map const *map);
void pcap_group_map_free(struct pcap_group_map *map);
int  pcap_group_map_set (struct pcap_group_map *map, const char *dev, int group);
int  pcap_group_map_get (struct pcap_group_map const *map, const char *dev);


#endif /* pcap_config_h */
