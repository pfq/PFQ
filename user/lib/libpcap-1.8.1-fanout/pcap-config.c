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


#include <pcap.h>
#include <pcap-int.h>

#include "pcap-config.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <errno.h>

#ifdef PCAP_SUPPORT_PFQ
#include <pfq/pfq.h>
#endif


extern char **environ;

struct pcap_conf_key pcap_conf_keys[] =
{
	PCAP_CONF_KEY(def_group)
,	PCAP_CONF_KEY(fanout)
,	PCAP_CONF_KEY(caplen)
#ifdef PCAP_SUPPORT_PFQ
,	PCAP_CONF_KEY(pfq_rx_slots)
,	PCAP_CONF_KEY(pfq_tx_slots)
,	PCAP_CONF_KEY(pfq_tx_sync)
,	PCAP_CONF_KEY(pfq_tx_hw_queue)
,	PCAP_CONF_KEY(pfq_tx_idx_thread)
,	PCAP_CONF_KEY(pfq_vlan)
#endif
};

static size_t pcap_conf_keys_eof = sizeof(pcap_conf_keys)/sizeof(pcap_conf_keys[0]);


struct pcap_config
pcap_config_default(pcap_t *handle)
{
	return (struct pcap_config)
	{
		.def_group		= -1
	,	.group_map		= {{[0 ... PCAP_FANOUT_GROUP_MAX-1] = {NULL, -1}}, 0 }
	,	.fanout			= { [0 ... PCAP_FANOUT_GROUP_DEFAULT] = NULL }
	,	.caplen			= handle->snapshot
#ifdef PCAP_SUPPORT_PFQ
	,	.pfq_rx_slots		= 4096
	,	.pfq_tx_slots		= 4096
	,	.pfq_tx_sync		= 1
	,	.pfq_tx_async		= 0
	,	.pfq_tx_hw_queue	= {-1, -1, -1, -1}
	,	.pfq_tx_idx_thread	= { Q_NO_KTHREAD, Q_NO_KTHREAD, Q_NO_KTHREAD, Q_NO_KTHREAD }
	,	.pfq_vlan		= {[0 ... PCAP_FANOUT_GROUP_DEFAULT] = NULL }
#endif
	};
}


void
pcap_group_map_dump(struct pcap_group_map const *map)
{
	int n = 0;
	for(; n < map->size; n++)
		fprintf(stderr, "libpcap: config group for dev '%s' = %d\n", map->entry[n].dev, map->entry[n].group);
}


void
pcap_group_map_free(struct pcap_group_map *map)
{
	int n = 0;
	for(; n < map->size; n++)
		free(map->entry[n].dev);
}


int
pcap_group_map_set(struct pcap_group_map *map, const char *dev, int group)
{
	int n = 0;
	for(; n < map->size; n++) {
		if (strcmp(map->entry[n].dev, dev) == 0)
			break;
	}

	if (n == PCAP_FANOUT_GROUP_MAX)
		return -1;

	free(map->entry[n].dev);
	map->entry[n].dev = strdup(dev);
	map->entry[n].group = group;

	if (n == map->size)
		map->size++;

	return 0;
}


int
pcap_group_map_get(struct pcap_group_map const *map, const char *dev)
{
	int n = 0;
	for(; n < map->size; n++) {
		if (strcmp(map->entry[n].dev, dev) == 0)
			return map->entry[n].group;
	}
	return -1;
}


static void
pcap_warn_if(int index, const char *filename, const char *key)
{
	if (index != PCAP_FANOUT_GROUP_DEFAULT)
		fprintf(stderr, "libpcap:%s: key %s: group ignored!\n", filename, key);
}


int
pcap_parse_integers(int *out, size_t max, const char *in)
{
	size_t n = 0; int ret = 0;

	int store_int(const char *num) {
		if (n < max) {
			out[n++] = atoi(num);
			ret++;
		}
		return 0;
	}

	if (pcap_string_for_each_token(in, ",", store_int) < 0)
		return -1;
	return ret;
}


int
pcap_string_for_each_token(const char *ds, const char *sep, pcap_string_handler_t handler)
{
        char * copy = strdup(ds);
        char *str, *token, *saveptr;
        int i, ret = 0;

        for (i = 1, str = copy; ; i++, str = NULL)
        {
                token = strtok_r(str, sep, &saveptr);
                if (token == NULL)
                        break;
                if (handler(token) < 0) {
		        ret = PCAP_ERROR;
			break;
		}
        }

        free(copy);
	return ret;
}


char *
pcap_string_first_token(const char *str, const char *sep)
{
	char *end;

	if ((end = strstr(str, sep))) {
		char *ret = (char *)malloc(end - str + 1);
		strncpy(ret, str, end - str);
		ret[end - str] = '\0';
		return ret;
	}

	return strdup(str);
}


char *
pcap_string_trim(char *str)
{
	int i = 0, j = strlen(str) - 1;

	while (isspace(str[i]) && str[i] != '\0')
		i++;
	while (j >= 0 && isspace(str[j]))
		j--;

	str[j+1] = '\0';
	return str+i;
}


char *
pcap_string_append(char *str1, const char *str2)
{
	char *ret;
	if (str1) {
		ret = (char *)realloc(str1, strlen(str1) + strlen(str2) + 1);
		strcat(ret, str2);
	}
	else {
		ret = (char *)malloc(strlen(str2) + 1);
		strcpy(ret, str2);
	}
	return ret;
}


static const char *
pcap_conf_get_key_name(char const *key)
{
	static __thread char storage[64];
	char * p = strchr(key, '@');
	int len;
	if (p == NULL)
		return key;

	len =  min(63, p - key);
	strncpy(storage, key, len);
	storage[len] = '\0';
	return storage;
}

static int
pcap_conf_get_key_index(char const *key)
{
	char * p = strchr(key, '@');
	if (p == NULL)
		return -1;
	return atoi(p+1);
}

static int
pcap_conf_find_key(const char *key, int *index)
{
	char const *this_key;
        int n;

	this_key = pcap_conf_get_key_name(key);
        *index = pcap_conf_get_key_index(key);

	for(n = 0; n < pcap_conf_keys_eof ; n++)
	{
		if (strcasecmp(pcap_conf_keys[n].value, this_key) == 0)
			return n;
	}
	return -1;
}


char *
pcap_getenv_name(char const *var)
{
	static __thread char name[64];
	char * end = strchr(var, '=');
	if (end) {
		strncpy(name, var, (size_t)min(63,end-var));
		name[min(63,end-var)] = '\0';
	}
	else {
		strcpy(name, var);
	}
	return name;
}

char *
pcap_getenv_value(char const *var)
{
	char *eq = strchr(var, '=');
	return eq ? eq+1 : NULL;
}


char **
pcap_getenv(char const *name)
{
	static __thread char *env[64];
        char **cur = environ;
	int size = 0;

	while (*cur && size < 64) {
		if (strncmp(*cur, name, strlen(name)) == 0) {
			env[size++] = *cur;
		}
		cur++;
	}
	env[size] = NULL;
	return env;
}



int
pcap_parse_config(struct pcap_config *opt, const char *filename)
{
	char line[1024];
	FILE *file;
	int rc = 0, n;

	file = fopen(filename, "r");
	if (!file) {
		fprintf(stderr, "libpcap: could not open '%s' file!\n", filename);
		rc = -1; goto err;
	}

	for(n = 0; fgets(line, sizeof(line), file); n++) {

		char *key = NULL, *value = NULL, *tkey;
		int ktype, index, ret;

		ret = sscanf(line, "%m[^=]=%m[^\n]",&key, &value);
		if (ret < 0) {
			fprintf(stderr, "libcap:%s: parse error at: %s\n", filename, key);
			rc = -1; goto next;
		}

		if (ret == 0)
			goto next;

		/* ret > 0 */

		tkey = pcap_string_trim(key);
		if (strlen(tkey) == 0)
			continue;

		/*  strlen > 0 */

		if (line[0] == '>') {
			opt->fanout[PCAP_FANOUT_GROUP_DEFAULT] = pcap_string_append(opt->fanout[PCAP_FANOUT_GROUP_DEFAULT], line+1);
			opt->fanout[PCAP_FANOUT_GROUP_DEFAULT] = pcap_string_append(opt->fanout[PCAP_FANOUT_GROUP_DEFAULT], "\n");
			continue;
		}

		if (tkey[0] == '#') /* skip comments */
			continue;

		if (strncasecmp(tkey, "group_", 6) == 0)
		{
			char *dev = strdup(pcap_getenv_name(tkey + sizeof("group_")-1));
			if (pcap_group_map_set(&opt->group_map, dev, atoi(value)) < 0) {
				fprintf(stderr, "libpcap:%s: '%s': group map error!\n", filename, tkey);
				rc = -1;
				goto next;
			}
			continue;
		}

		ktype = pcap_conf_find_key(tkey, &index);

		index = index == -1 ?  PCAP_FANOUT_GROUP_DEFAULT : index;

		switch(ktype)
		{
			case PCAP_CONF_KEY_def_group: {
				pcap_warn_if(index, filename, tkey);
				opt->def_group = atoi(value);
			} break;
			case PCAP_CONF_KEY_fanout: {
				free (opt->fanout[index]);
				opt->fanout[index] = strdup(pcap_string_trim(value));
			} break;
			case PCAP_CONF_KEY_caplen: {
				pcap_warn_if(index, filename, tkey);
				opt->caplen = atoi(value);
			} break;
#ifdef PCAP_SUPPORT_PFQ
			case PCAP_CONF_KEY_pfq_rx_slots: {
				pcap_warn_if(index, filename, tkey);
				opt->pfq_rx_slots  = atoi(value);
			} break;
			case PCAP_CONF_KEY_pfq_tx_slots: {
				pcap_warn_if(index, filename, tkey);
				opt->pfq_tx_slots  = atoi(value);
			} break;
			case PCAP_CONF_KEY_pfq_tx_sync: {
				pcap_warn_if(index, filename, tkey);
				opt->pfq_tx_sync   = atoi(value);
			}  break;
			case PCAP_CONF_KEY_pfq_tx_hw_queue: {
				pcap_warn_if(index, filename, tkey);
				if (pcap_parse_integers(opt->pfq_tx_hw_queue, 4, value) < 0) {
					fprintf(stderr, "libpcap:%s: parse error at: %s\n", filename, tkey);
					rc = -1;
				}
			} break;
			case PCAP_CONF_KEY_pfq_tx_idx_thread: {
				pcap_warn_if(index, filename, tkey);
				if (pcap_parse_integers(opt->pfq_tx_idx_thread, 4, value) < 0) {
					fprintf(stderr, "libpcap:%s: parse error at: %s\n", filename, tkey);
					rc = -1;
				}
			} break;

			case PCAP_CONF_KEY_pfq_vlan: {
				free (opt->pfq_vlan[index]);
				opt->pfq_vlan[index] = strdup(pcap_string_trim(value));
			} break;
#endif
			case PCAP_CONF_KEY_error:
			default: {
				fprintf(stderr, "libpcap:%s: parse error at: %s (invalid keyword)\n", filename, tkey);
				rc = -1;
			} break;
		}
	next:
		free(key);
		free(value);
		if (rc == -1)
			break;
	}

	fclose(file);

err:
	return rc;
}


