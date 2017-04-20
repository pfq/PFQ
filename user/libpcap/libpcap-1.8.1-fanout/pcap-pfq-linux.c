/*
 * Copyright (c) 2011-16 Nicola Bonelli <nicola@pfq.io>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * PFQ sniffing API implementation for Linux platform
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#define _GNU_SOURCE
#include <string.h>

#include <pcap-int.h>
#include <assert.h>
#include <errno.h>
#include <stdlib.h>

#include <pcap.h>
#include "pcap/sll.h"
#include "pcap/vlan.h"

#include <linux/filter.h>
#include <linux/if_ether.h>
#include <linux/pf_q.h>

#include <pfq/pfq.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <sys/mman.h>
#include <poll.h>
#include <ctype.h>

#include "pcap-pfq-linux.h"


static void pfq_cleanup_linux(pcap_t *);
static	int pfq_activate_linux(pcap_t *);
static	int pfq_inject_linux(pcap_t *, const void *, size_t);
static	int pfq_setdirection_linux(pcap_t *, pcap_direction_t);
static	int pfq_read_linux(pcap_t *, int, pcap_handler, u_char *);
static	int pfq_stats_linux(pcap_t *, struct pcap_stat *);


#define MUST_CLEAR_PROMISC	0x00000001
#define DEV_SEP			"^"

pcap_t *
pfq_create(char *ebuf, size_t size)
{
	pcap_t *handle;

	handle = pcap_create_common(ebuf, size);
	if (handle == NULL)
		return NULL;

	handle->activate_op = pfq_activate_linux;
	return handle;
}


static int
set_kernel_filter(pcap_t *handle, struct sock_fprog *fcode)
{
	struct pcap_pfq_linux *handlep = handle->priv;
	return pfq_group_fprog(handlep->q, handle->group, fcode);
}


static int
reset_kernel_filter(pcap_t *handle)
{
	struct pcap_pfq_linux *handlep = handle->priv;
	return pfq_group_fprog_reset(handlep->q, handle->group);
}

static int fix_offset(struct bpf_insn *p);

static int
fix_program(pcap_t *handle, struct sock_fprog *fcode, int is_mmapped)
{
	struct pcap_pfq_linux *handlep = handle->priv;
	size_t prog_size;
	int i;
	struct bpf_insn *p;
	struct bpf_insn *f;
	int len;

	/*
	 * Make a copy of the filter, and modify that copy if
	 * necessary.
	 */
	prog_size = sizeof(*handle->fcode.bf_insns) * handle->fcode.bf_len;
	len = handle->fcode.bf_len;
	f = (struct bpf_insn *)malloc(prog_size);
	if (f == NULL) {
		pcap_snprintf(handle->errbuf, PCAP_ERRBUF_SIZE,
			 "malloc: %s", pcap_strerror(errno));
		return -1;
	}
	memcpy(f, handle->fcode.bf_insns, prog_size);
	fcode->len = len;
	fcode->filter = (struct sock_filter *) f;

	for (i = 0; i < len; ++i) {
		p = &f[i];
		/*
		 * What type of instruction is this?
		 */
		switch (BPF_CLASS(p->code)) {

		case BPF_RET:
			/*
			 * It's a return instruction; are we capturing
			 * in memory-mapped mode?
			 */
			if (!is_mmapped) {
				/*
				 * No; is the snapshot length a constant,
				 * rather than the contents of the
				 * accumulator?
				 */
				if (BPF_MODE(p->code) == BPF_K) {
					/*
					 * Yes - if the value to be returned,
					 * i.e. the snapshot length, is
					 * anything other than 0, make it
					 * MAXIMUM_SNAPLEN, so that the packet
					 * is truncated by "recvfrom()",
					 * not by the filter.
					 *
					 * XXX - there's nothing we can
					 * easily do if it's getting the
					 * value from the accumulator; we'd
					 * have to insert code to force
					 * non-zero values to be
					 * MAXIMUM_SNAPLEN.
					 */
					if (p->k != 0)
						p->k = MAXIMUM_SNAPLEN;
				}
			}
			break;

		case BPF_LD:
		case BPF_LDX:
			/*
			 * It's a load instruction; is it loading
			 * from the packet?
			 */
			switch (BPF_MODE(p->code)) {

			case BPF_ABS:
			case BPF_IND:
			case BPF_MSH:
				/*
				 * Yes; are we in cooked mode?
				 */
				if (handlep->cooked) {
					/*
					 * Yes, so we need to fix this
					 * instruction.
					 */
					if (fix_offset(p) < 0) {
						/*
						 * We failed to do so.
						 * Return 0, so our caller
						 * knows to punt to userland.
						 */
						return 0;
					}
				}
				break;
			}
			break;
		}
	}
	return 1;	/* we succeeded */
}

static int
fix_offset(struct bpf_insn *p)
{
	/*
	 * What's the offset?
	 */
	if (p->k >= SLL_HDR_LEN) {
		/*
		 * It's within the link-layer payload; that starts at an
		 * offset of 0, as far as the kernel packet filter is
		 * concerned, so subtract the length of the link-layer
		 * header.
		 */
		p->k -= SLL_HDR_LEN;
	} else if (p->k == 0) {
		/*
		 * It's the packet type field; map it to the special magic
		 * kernel offset for that field.
		 */
		p->k = SKF_AD_OFF + SKF_AD_PKTTYPE;
	} else if (p->k == 14) {
		/*
		 * It's the protocol field; map it to the special magic
		 * kernel offset for that field.
		 */
		p->k = SKF_AD_OFF + SKF_AD_PROTOCOL;
	} else if ((bpf_int32)(p->k) > 0) {
		/*
		 * It's within the header, but it's not one of those
		 * fields; we can't do that in the kernel, so punt
		 * to userland.
		 */
		return -1;
	}
	return 0;
}



static int
pfq_setfilter_linux(pcap_t *handle, struct bpf_program *filter)
{
	struct pcap_pfq_linux *handlep = handle->priv;
	struct sock_fprog fcode;
	int can_filter_in_kernel;
	int err = 0;

	if (!handle)
		return -1;
	if (!filter) {
	        strlcpy(handle->errbuf, "[PFQ] setfilter: No filter specified",
			PCAP_ERRBUF_SIZE);
		return -1;
	}

	/* Make our private copy of the filter */

	if (install_bpf_program(handle, filter) < 0)
		/* install_bpf_program() filled in errbuf */
		return -1;

	/*
	 * Run user level packet filter by default. Will be overriden if
	 * installing a kernel filter succeeds.
	 */
	handlep->filter_in_userland = 1;

	switch (fix_program(handle, &fcode, 1)) {

	case -1:
	default:
		/*
		 * Fatal error; just quit.
		 * (The "default" case shouldn't happen; we
		 * return -1 for that reason.)
		 */
		return -1;

	case 0:
		/*
		 * The program performed checks that we can't make
		 * work in the kernel.
		 */
		can_filter_in_kernel = 0;
		break;

	case 1:
		/*
		 * We have a filter that'll work in the kernel.
		 */
		can_filter_in_kernel = 1;
		break;
	}

	if (can_filter_in_kernel) {

		if ((err = set_kernel_filter(handle, &fcode)) == 0) {

			/* Installation succeded - using kernel filter. */
			handlep->filter_in_userland = 0;
		}
		else if (err == -1) {	/* Non-fatal error */

			/*
			 * Print a warning if we weren't able to install
			 * the filter for a reason other than "this kernel
			 * isn't configured to support socket filters.
			 */
			if (errno != ENOPROTOOPT && errno != EOPNOTSUPP) {
				fprintf(stderr,
				    "[PFQ] Kernel filter failed: %s\n",
					pcap_strerror(errno));
			}
		}
	}
	else
		fprintf(stderr, "[PFQ] could not set BPF filter in kernel!\n");

	/*
	 * If we're not using the kernel filter, get rid of any kernel
	 * filter that might've been there before, e.g. because the
	 * previous filter could work in the kernel, or because some other
	 * code attached a filter to the socket by some means other than
	 * calling "pcap_setfilter()".  Otherwise, the kernel filter may
	 * filter out packets that would pass the new userland filter.
	 */
	if (handlep->filter_in_userland)
		reset_kernel_filter(handle);

	/*
	 * Free up the copy of the filter that was made by "fix_program()".
	 */
	if (fcode.filter != NULL)
		free(fcode.filter);

	if (err == -2)
		/* Fatal error */
		return -1;

	return 0;
}


/***********************************************/


static char *
pfq_get_config_file(const char *fullname)
{
	char *end, *conf;
	if (!fullname)
		return NULL;
	conf = strstr(fullname, "pfq/");
	if (conf == NULL)
		return NULL;
	conf = strdup(conf+4);
	if ((end = strchr(conf, ':'))) {
		*end = '\0';
	}
	return conf;
}


static char *
pfq_get_devname(const char *fullname)
{
	char *dev;
	if (!fullname)
		return NULL;
	dev = strstr(fullname, "pfq");
	if (!dev)
		return strdup(fullname);
	if ((dev = strchr(dev, ':')))
		return strdup(dev+1);
	return NULL;
}



static long int
linux_if_drops(const char * if_name)
{
	char buffer[512];
	char * bufptr;
	FILE * file;
	int field_to_convert = 3, if_name_sz = strlen(if_name);
	long int dropped_pkts = 0;

	file = fopen("/proc/net/dev", "r");
	if (!file)
		return 0;

	while (!dropped_pkts && fgets( buffer, sizeof(buffer), file ))
	{
		/* 	search for 'bytes' -- if its in there, then
			that means we need to grab the fourth field. otherwise
			grab the third field. */
		if (field_to_convert != 4 && strstr(buffer, "bytes")) {

			field_to_convert = 4;
			continue;
		}

		/* find iface and make sure it actually matches -- space before the name and : after it */
		if ((bufptr = strstr(buffer, if_name)) &&
			(bufptr == buffer || *(bufptr-1) == ' ') &&
			*(bufptr + if_name_sz) == ':')
		{
			bufptr = bufptr + if_name_sz + 1;

			/* grab the nth field from it */
			while( --field_to_convert && *bufptr != '\0')
			{
				while (*bufptr != '\0' && *(bufptr++) == ' ');
				while (*bufptr != '\0' && *(bufptr++) != ' ');
			}

			/* get rid of any final spaces */
			while (*bufptr != '\0' && *bufptr == ' ') bufptr++;

			if (*bufptr != '\0')
				dropped_pkts = strtol(bufptr, NULL, 10);

			break;
		}
	}

	fclose(file);
	return dropped_pkts;
}


static size_t
pfq_count_tx_thread(struct pcap_config const *opt)
{
	size_t n, tx = 0;
        for(n = 0; n < 4; n++) {
		if (opt->pfq_tx_idx_thread[n] != Q_NO_KTHREAD)
			tx++;
	}
	return tx;
}


static
int pfq_fanout(pcap_t *handle, int group, const char *fanout)
{
	struct pcap_pfq_linux *handlep = handle->priv;
	struct stat s;

	if (stat(fanout, &s) == 0) { /* fanout is a filepath */

		fprintf(stderr, "[PFQ] loading pfq source '%s' for group %d\n", fanout, group);
		if (pfq_set_group_computation_from_file( handlep->q
						       , group
						       , fanout) < 0) {

			snprintf(handle->errbuf, PCAP_ERRBUF_SIZE,
				 "[PFQ] error: %s", pfq_error(handlep->q));
			return PCAP_ERROR;
		}

	} else {
		fprintf(stderr, "[PFQ] loading in-line pfq '%s' for group %d\n", fanout, group);
		if (pfq_set_group_computation_from_string( handlep->q
							 , group
							 , fanout) < 0) {
			snprintf(handle->errbuf, PCAP_ERRBUF_SIZE,
				 "[PFQ] error: %s", pfq_error(handlep->q));
			return PCAP_ERROR;
		}
	}
}


static int
pfq_parse_env(struct pcap_config *opt)
{
	char *var, **vars;

	if ((var = getenv("PFQ_DEF_GROUP"))   ||
	    (var = getenv("PFQ_GROUP"))       ||
	    (var = getenv("PCAP_DEF_GROUP"))  ||
	    (var = getenv("PCAP_GROUP")))
		opt->def_group = atoi(var);

	if ((var = getenv("PFQ_LANG"))	  ||
	    (var = getenv("PFQ_FANOUT"))  ||
	    (var = getenv("PCAP_FANOUT"))
	   )
	{
		free(opt->fanout[PCAP_FANOUT_GROUP_DEFAULT]);
		opt->fanout[PCAP_FANOUT_GROUP_DEFAULT] = strdup(var);
	}

        if ((vars = pcap_getenv("PFQ_GROUP_")) ||
            (vars = pcap_getenv("PCAP_GROUP_")))
	{
		for(; *vars; vars++)
		{
			char *p, *dev = strdup(pcap_getenv_name(*vars + sizeof("PFQ_GROUP_")-1));
			for(p = dev; *p != '\0'; ++p)
				if (*p == '_')
					*p = ':';
			if (pcap_group_map_set(&opt->group_map, dev, atoi(pcap_getenv_value(*vars))) < 0) {
				fprintf(stderr, "[PFQ] %s: group map error!\n", *vars);
				return -1;
			}

		}
	}

	if ((var = getenv("PFQ_CAPLEN")) ||
	    (var = getenv("PCAP_CAPLEN")))
		opt->caplen = atoi(var);

	if ((var = getenv("PFQ_RX_SLOTS")))
		opt->pfq_rx_slots = atoi(var);

	if ((var = getenv("PFQ_TX_SLOTS")))
		opt->pfq_tx_slots = atoi(var);

	if ((var = getenv("PFQ_TX_SYNC")))
		opt->pfq_tx_sync = atoi(var);

	if ((var = getenv("PFQ_VLAN")))
		opt->pfq_vlan[PCAP_FANOUT_GROUP_DEFAULT] = var;

	if ((var = getenv("PFQ_TX_HW_QUEUE"))) {
		if (pcap_parse_integers(opt->pfq_tx_hw_queue, 4, var) < 0) {
			fprintf(stderr, "[PFQ] PFQ_TX_HW_QUEUE parse error!\n");
			return -1;
		}
	}

	if ((var = getenv("PFQ_TX_IDX_THREAD"))) {
		if (pcap_parse_integers(opt->pfq_tx_idx_thread, 4, var) < 0) {
			fprintf(stderr, "[PFQ] PFQ_TX_IDX_THREAD parse error!\n");
			return -1;
		}
	}

	return 0;
}



static char *
pfq_get_real_devname(const char *var)
{
	static __thread char name[64];
	char * end = strchr(var, ':');
	if (end) {
		size_t len = (size_t)min(63, end-var);
		strncpy(name, var, len);
		name[len] = '\0';
	}
	else {
		strcpy(name, var);
	}
	return name;
}


static int
pfq_activate_socket_for_device(pcap_t *handle, const char *device)
{
	struct pcap_pfq_linux *handlep = handle->priv;
	int group;

	group = pcap_group_map_get(&handle->opt.config.group_map, handlep->device);
	if (group == -1)
		group = handle->opt.config.def_group;

	/*
	 * Bind Rx to groups
	 */

	if (group != -1) {

		int bind_group(const char *_dev)
		{
			const char *dev = pfq_get_real_devname(_dev);

			fprintf(stderr, "[PFQ] binding Rx group %d on dev %s...\n", group, dev);

			if (pfq_bind_group(handlep->q, group, dev, Q_ANY_QUEUE) == -1) {
				fprintf(stderr, "[PFQ] error: %s\n", pfq_error(handlep->q));
			}
			return 0;
		}

		handlep->q = pfq_open_nogroup(min(handle->snapshot, handle->opt.config.caplen),
					      handle->opt.config.pfq_rx_slots,
					      handle->opt.config.pfq_tx_slots);
		if (handlep->q == NULL) {
			snprintf(handle->errbuf, PCAP_ERRBUF_SIZE, "%s", pfq_error(handlep->q));
			return -1;
		}

		if (pfq_join_group(handlep->q, group, Q_CLASS_DEFAULT, Q_POLICY_GROUP_SHARED) < 0) {
			fprintf(stderr, "[PFQ] error: %s\n", pfq_error(handlep->q));
		}

		/* bind to device(es) if specified */

		if (device && strcmp(device, "any") != 0) {
			if (pcap_string_for_each_token(device, DEV_SEP, bind_group) < 0)
				return -1;
		}

		return group;
	}
	else
	{
		int bind_socket(const char *_dev)
		{
			const char *dev = pfq_get_real_devname(_dev);

			fprintf(stderr, "[PFQ] binding socket on dev %s...\n", dev);

			if (pfq_bind(handlep->q, dev, Q_ANY_QUEUE) == -1) {
				fprintf(stderr, "[PFQ] error: %s\n", pfq_error(handlep->q));
			}
			return 0;
		}

		handlep->q = pfq_open_group(Q_CLASS_DEFAULT, Q_POLICY_GROUP_SHARED,
						  min(handle->snapshot, handle->opt.config.caplen),
						  handle->opt.config.pfq_rx_slots,
						  handle->opt.config.pfq_tx_slots);
		if (handlep->q == NULL) {
			snprintf(handle->errbuf, PCAP_ERRBUF_SIZE, "%s", pfq_error(handlep->q));
			return -1;
		}

		/* bind to device(es) if specified */

		if (device && strcmp(device, "any") != 0) {
			if (pcap_string_for_each_token(device, DEV_SEP, bind_socket) < 0)
				return -1;
		}
	}

	group = pfq_group_id(handlep->q);
	if (group == -1) {
		snprintf(handle->errbuf, PCAP_ERRBUF_SIZE, "%s", pfq_error(handlep->q));
		return -1;
	}

	return group;
}


static int
pfq_activate_linux(pcap_t *handle)
{
	struct pcap_pfq_linux *handlep = handle->priv;

	char *device = NULL, *config = NULL;
        const int maxlen = 1514;
	char *first_dev;
	int group;

	handle->opt.config = pcap_config_default(handle);
	handle->linktype = DLT_EN10MB;
	device = pfq_get_devname(handle->opt.device);

	fprintf(stderr, "[PFQ] socket on device %s...\n", device);

	config = pfq_get_config_file(handle->opt.device);
	if (config == NULL) {
		char *conf = getenv("PFQ_CONFIG") ? : getenv("PCAP_CONFIG");
		if (conf)
			config = strdup(conf);
	}

	if (config != NULL) {
		fprintf(stderr, "[PFQ] parsing config file %s...\n", config);

		if (pcap_parse_config(&handle->opt.config, config) == -1) {
			snprintf(handle->errbuf, PCAP_ERRBUF_SIZE, "pfq: config error");
			return PCAP_ERROR;
		}

		free(config);
	}

	if (pfq_parse_env(&handle->opt.config) == -1) {
		snprintf(handle->errbuf, PCAP_ERRBUF_SIZE, "pfq: environ variable(s) error!");
		return PCAP_ERROR;
	}

        if (handle->opt.config.caplen > maxlen || handle->opt.config.caplen == 0) {
                fprintf(stderr, "[PFQ] capture length forced to %d\n", maxlen);
                handle->opt.config.caplen = maxlen;
        }

	if (handle->opt.buffer_size/handle->opt.config.caplen > handle->opt.config.pfq_rx_slots)
		handle->opt.config.pfq_rx_slots = handle->opt.buffer_size/handle->opt.config.caplen;

	fprintf(stderr, "[PFQ] config caplen = %d, rx_slots = %d, tx_slots = %d, tx_sync = %d\n",
		handle->opt.config.caplen,
		handle->opt.config.pfq_rx_slots,
		handle->opt.config.pfq_tx_slots,
		handle->opt.config.pfq_tx_sync);


	pcap_group_map_dump(&handle->opt.config.group_map);

	fprintf(stderr, "[PFQ] config group default %d\n", handle->opt.config.def_group);

	handle->read_op		= pfq_read_linux;
	handle->inject_op	= pfq_inject_linux;
	handle->setfilter_op	= pfq_setfilter_linux;
	handle->setdirection_op	= pfq_setdirection_linux;
	handle->getnonblock_op	= pcap_getnonblock_fd;
	handle->setnonblock_op	= pcap_setnonblock_fd;
	handle->stats_op	= pfq_stats_linux;
	handle->cleanup_op	= pfq_cleanup_linux;
	handle->fanout_op	= pfq_fanout;
	handle->set_datalink_op	= NULL;	/* can't change data link type */

	handlep->q		= NULL;
	handlep->current	= NULL;

	pfq_net_queue_init(&handlep->nqueue);
	handlep->ifs_promisc = 0;

	handle->fd = socket(AF_INET, SOCK_DGRAM, 0);
	if (handle->fd == -1) {
		snprintf(handle->errbuf, PCAP_ERRBUF_SIZE,
			 "socket: %s", pcap_strerror(errno));
		if (errno == EPERM || errno == EACCES) {
			/*
			 * You don't have permission to open the
			 * socket.
			 */
			return PCAP_ERROR_PERM_DENIED;
		} else {
			/*
			 * Other error.
			 */
			return PCAP_ERROR;
		}
	}

	/*
	 * The "any" device is a special device which causes us not
	 * to bind to a particular device and thus to look at all
	 * devices of a given group.
	 */

	/* handle promisc */

	if (handle->opt.promisc) {

		/* put all devic(es) in promisc mode */
                int n = 0;

		int set_promisc(const char *_dev)
		{
			struct ifreq ifr;

			const char *dev = pfq_get_real_devname(_dev);

			memset(&ifr, 0, sizeof(ifr));
			strlcpy(ifr.ifr_name, dev, sizeof(ifr.ifr_name));
			if (ioctl(handle->fd, SIOCGIFFLAGS, &ifr) == -1) {
				snprintf(handle->errbuf, PCAP_ERRBUF_SIZE,
						"SIOCGIFFLAGS: %s", pcap_strerror(errno));
				return PCAP_ERROR;
			}

			fprintf(stderr, "[PFQ] set promisc on dev %s...\n", dev);

			if ((ifr.ifr_flags & IFF_PROMISC) == 0) {

				/*
				 * Promiscuous mode isn't currently on,
				 * so turn it on, and remember that
				 * we should turn it off when the
				 * pcap_t is closed.
				 */

				/*
				 * If we haven't already done so, arrange
				 * to have "pcap_close_all()" called when
				 * we exit.
				 */
				if (!pcap_do_addexit(handle)) {
					/*
					 * "atexit()" failed; don't put
					 * the interface in promiscuous
					 * mode, just give up.
					 */
					return PCAP_ERROR;
				}


				ifr.ifr_flags |= IFF_PROMISC;
				if (ioctl(handle->fd, SIOCSIFFLAGS, &ifr) == -1) {
					snprintf(handle->errbuf, PCAP_ERRBUF_SIZE,
							"SIOCSIFFLAGS: %s",
							pcap_strerror(errno));
					return PCAP_ERROR;
				}

				handlep->ifs_promisc |= (1 << n);
				handlep->must_do_on_close |= MUST_CLEAR_PROMISC;
			}

			n++;
			return 0;
		}

		if (device && strcmp(device, "any")) {
			if (pcap_string_for_each_token(device, DEV_SEP, set_promisc) < 0) {
				goto fail;
			}
		}
	}

	if (device) {
		handlep->device = strdup(device);
		if (handlep->device == NULL) {
			snprintf(handle->errbuf, PCAP_ERRBUF_SIZE, "strdup: %s",
				 pcap_strerror(errno) );
			goto fail;
		}
	}

	/*
	 * If we're in promiscuous mode, then we probably want
	 * to see when the interface drops packets too, so get an
	 * initial count from /proc/net/dev
	 */

	if (handle->opt.promisc)
		handlep->proc_dropped = handlep->device ? linux_if_drops(handlep->device) : 0;

	handle->group = pfq_activate_socket_for_device(handle, device);
	if (handle->group == -1) {
		goto fail;
	}

	fprintf(stderr, "[PFQ] socket (%d) is using Rx group %d\n", pfq_id(handlep->q), handle->group);

	/*
	 * Bind TX to device/queue
	 */

	if (device && strcmp(device, "any"))
	{
		if ((first_dev = pcap_string_first_token(device, DEV_SEP))) {

			size_t tot, idx;

			tot = pfq_count_tx_thread(&handle->opt.config);
			if (tot) {
				fprintf(stderr, "[PFQ] enabling %zu Tx async on dev %s...\n", tot, first_dev);

				handle->opt.config.pfq_tx_async = 1;

				for(idx = 0; idx < tot; idx++)
				{
					fprintf(stderr, "[PFQ] binding Tx on dev %s, hw queue %d, Tx thread %d\n",
						first_dev, handle->opt.config.pfq_tx_hw_queue[idx], handle->opt.config.pfq_tx_idx_thread[idx]);

					if (pfq_bind_tx(handlep->q, first_dev,
							handle->opt.config.pfq_tx_hw_queue[idx],
							handle->opt.config.pfq_tx_idx_thread[idx]) < 0) {
						fprintf(stderr, "[PFQ] error: %s\n", pfq_error(handlep->q));
						goto fail;
					}
				}
			}
			else {
				fprintf(stderr, "[PFQ] enabling Tx on dev %s, hw queue %d\n", first_dev,
					handle->opt.config.pfq_tx_hw_queue[0]);
				if (pfq_bind_tx(handlep->q, first_dev, handle->opt.config.pfq_tx_hw_queue[0], -1)) {
					fprintf(stderr, "[PFQ] error: %s\n", pfq_error(handlep->q));
					goto fail;
				}
			}

			free(first_dev);
		}
	}

	/*
	 * Set pfq-lang computation
	 */

	/* Fanout  */

	char *cur_fanout = handle->opt.config.fanout[handle->group] ?
			   handle->opt.config.fanout[handle->group] :
			   handle->opt.config.fanout[PCAP_FANOUT_GROUP_DEFAULT];

	if (cur_fanout) {
		if (pfq_fanout(handle, handle->group, cur_fanout) < 0) {
			goto fail;
		}
	}

	/*
	 * Set vlan filters
	 */

	char *cur_vlan = handle->opt.config.pfq_vlan[handle->group] ?
			 handle->opt.config.pfq_vlan[handle->group] :
			 handle->opt.config.pfq_vlan[PCAP_FANOUT_GROUP_DEFAULT];

	if (cur_vlan) {

                if (pfq_vlan_filters_enable(handlep->q, handle->group, 1) < 0) {

			fprintf(stderr, "[PFQ] error: %s\n", pfq_error(handlep->q));
                }

		int set_vlan_filter(const char *vid_)
		{
		        int vid = atoi(vid_);

			fprintf(stderr, "[PFQ] group %d setting vlan filer id=%d\n", handle->group, vid);

			if (pfq_vlan_set_filter(handlep->q, handle->group, vid)  == -1) {
				fprintf(stderr, "[PFQ] error: %s\n", pfq_error(handlep->q));
			}
			return 0;
		}

		if (pcap_string_for_each_token(cur_vlan, ",", set_vlan_filter) < 0)
                        goto fail;
        }

	/*
	 * Enable timestamping
	 */

	if (pfq_timestamping_enable(handlep->q, 1) == -1) {
		snprintf(handle->errbuf, PCAP_ERRBUF_SIZE, "%s", pfq_error(handlep->q));
		goto fail;
	}

	/*
	 * Enable PFQ socket
	 */

	if (pfq_enable(handlep->q) == -1) {
		snprintf(handle->errbuf, PCAP_ERRBUF_SIZE, "%s", pfq_error(handlep->q));
		goto fail;
	}

	/* handle->selectable_fd = pfq_get_fd(handlep->q); */

	handle->selectable_fd = -1;
	return 0;

fail:
	pfq_cleanup_linux(handle);
	return PCAP_ERROR;
}


static int
pfq_inject_linux(pcap_t *handle, const void * buf, size_t size)
{
	struct pcap_pfq_linux *handlep = handle->priv;
	int ret;

	if (handle->opt.config.pfq_tx_async)
		ret = pfq_send_async(handlep->q, buf, size, 1);
	else
		ret = pfq_send(handlep->q, buf, size, handle->opt.config.pfq_tx_sync, 1);
        if (ret == -1) {
		snprintf(handle->errbuf, PCAP_ERRBUF_SIZE, "%s", pfq_error(handlep->q));
		return PCAP_ERROR;
        }

	return ret;
}


static void
pfq_cleanup_linux(pcap_t *handle)
{
	struct pcap_pfq_linux *handlep = handle->priv;
	int n = 0;

	int clear_promisc(const char *dev)
	{
		struct ifreq ifr;

		if (!(handlep->ifs_promisc & (1 << n++)))
			return 0;

		memset(&ifr, 0, sizeof(ifr));
		strlcpy(ifr.ifr_name, dev, sizeof(ifr.ifr_name));
		if (ioctl(handle->fd, SIOCGIFFLAGS, &ifr) == -1) {
			fprintf(stderr,
					"Can't restore interface %s flags (SIOCGIFFLAGS failed: %s).\n"
					"Please adjust manually.\n"
					"Hint: This can't happen with Linux >= 2.2.0.\n",
					dev, strerror(errno));
		} else {
			if (ifr.ifr_flags & IFF_PROMISC) {
				/*
				 * Promiscuous mode is currently on;
				 * turn it off.
				 */

				fprintf(stderr, "[PFQ] clear promisc on dev %s...\n", dev);

				ifr.ifr_flags &= ~IFF_PROMISC;
				if (ioctl(handle->fd, SIOCSIFFLAGS,
							&ifr) == -1) {
					fprintf(stderr,
							"Can't restore interface %s flags (SIOCSIFFLAGS failed: %s).\n"
							"Please adjust manually.\n"
							"Hint: This can't happen with Linux >= 2.2.0.\n",
							dev,
							strerror(errno));
				}
			}
		}

		return 0;
	}

	if (handlep->must_do_on_close & MUST_CLEAR_PROMISC) {

		if (handlep->device && strcmp(handlep->device, "any") != 0) {
			pcap_string_for_each_token(handlep->device, DEV_SEP, clear_promisc);
		}
	}

	if(handlep->q) {
		fprintf(stderr, "[PFQ] close socket.\n");
		pfq_close(handlep->q);
		handlep->q = NULL;
	}

	close(handle->fd);

	free(handlep->device);
	handlep->device = NULL;
	pcap_cleanup_live_common(handle);
}


static int
pfq_read_linux(pcap_t *handle, int max_packets, pcap_handler callback, u_char *user)
{
	struct pcap_pfq_linux *handlep = handle->priv;

        int start = handlep->packets_read;
	pfq_iterator_t it = handlep->current;
	struct pfq_net_queue *nq = &handlep->nqueue;
	int n = max_packets;

        if (it == pfq_net_queue_end(nq)) {

		if (unlikely(pfq_read(handlep->q, nq, handlep->timeout > 0 ? handlep->timeout * 1000 : 1000000) < 0)) {
			snprintf(handle->errbuf, sizeof(handle->errbuf), "PFQ read error");
			return PCAP_ERROR;
		}
		it = handlep->current = pfq_net_queue_begin(nq);
	}

        /* process the queue */

	for(; (max_packets <= 0 || n > 0) && (it != pfq_net_queue_end(nq))
	    ;	it = pfq_net_queue_next(nq, it))
	{
		struct pfq_pcap_pkthdr pcap_h;
		struct pfq_pkthdr *h;
                uint16_t vlan_tci;
		const char *pkt;

		__builtin_prefetch(pfq_pkt_header(it), 0, 3);
		__builtin_prefetch((char *)(pfq_pkt_header(it))+64, 0, 3);
		__builtin_prefetch((char *)(pfq_pkt_header(it))+128, 0, 3);

		while (!pfq_pkt_ready(nq, it)) {
			if (unlikely(handle->break_loop)) {
				handle->break_loop = 0;
				return PCAP_ERROR_BREAK;
			}
			pfq_relax();
		}

		h = (struct pfq_pkthdr *)pfq_pkt_header(it);

		pcap_h.ts.tv_sec  = h->tstamp.tv.sec;
		pcap_h.ts.tv_usec = h->tstamp.tv.nsec / 1000;
		pcap_h.caplen     = h->caplen;
		pcap_h.len        = h->len;

		/* extended PFQ pcap header */

		memcpy(&pcap_h.info, &h->info, sizeof(struct pfq_pkthdr_info));

		/* Add 802.1Q header if present */

		pkt = pfq_pkt_data(it);

		if ((vlan_tci = h->info.vlan.tci) != 0) {

			struct vlan_tag *tag;

			pkt -= VLAN_TAG_LEN;

			memmove((char *)pkt, pkt + VLAN_TAG_LEN, 2 * ETH_ALEN);

			tag = (struct vlan_tag *)(pkt + 2 * ETH_ALEN);
			tag->vlan_tpid = htons(ETH_P_8021Q);
			tag->vlan_tci  = htons(vlan_tci);

			pcap_h.len += VLAN_TAG_LEN;
		}

		/* pass packet and header to the given callback */

		callback(user, (struct pcap_pkthdr *)&pcap_h, pkt);

		handlep->packets_read++;
		n--;
	}

	handlep->current = it;

	if (unlikely(handle->break_loop)) {
		handle->break_loop = 0;
		return PCAP_ERROR_BREAK;
	}

	return handlep->packets_read-start;
}


static int
pfq_setdirection_linux(pcap_t *handle, pcap_direction_t d)
{
        fprintf(stderr, "[PFQ] set direciton not support with PFQ.\n");
	return 0;
}


static int
pfq_stats_linux(pcap_t *handle, struct pcap_stat *stat)
{
	struct pcap_pfq_linux *handlep = handle->priv;
	struct pfq_stats qstats;
	long if_dropped = 0;

	if(pfq_get_stats(handlep->q, &qstats) < 0)
		return -1;

	if (handle->opt.promisc) {
		if_dropped = handlep->proc_dropped;
		handlep->proc_dropped = handlep->device ? linux_if_drops(handlep->device) : 0;
		handlep->stat.ps_ifdrop += (handlep->proc_dropped - if_dropped);
	}

	stat->ps_recv   = (u_int) qstats.recv;
	stat->ps_drop   = (u_int) qstats.lost;
	stat->ps_ifdrop = handlep->stat.ps_ifdrop;

	return 0;
}

