/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PFQ_LANG_QBUFF_H
#define PFQ_LANG_QBUFF_H


#include <engine/lang/monad.h>

#include <pfq/nethdr.h>


static inline int
next_ip_offset(struct qbuff const *buff, int offset, int tproto, int *proto)
{
	(void)buff;

	switch(tproto)
	{
	case IPPROTO_IPIP: {
		*proto = IPPROTO_IP;
		return offset;
	}
	case IPPROTO_IPV6: {
		*proto = IPPROTO_IPV6;
		return offset;
	}
	}

	return -1;
}


static inline int
qbuff_next_ip_offset(struct qbuff *buff, int offset, int *proto)
{
	switch(*proto)
	{
	case IPPROTO_NONE: {

		if (qbuff_eth_hdr(buff)->h_proto == __constant_htons(ETH_P_IP))
		{
			*proto = IPPROTO_IP;
			return qbuff_maclen(buff);
		}

		if (qbuff_eth_hdr(buff)->h_proto == __constant_htons(ETH_P_IPV6))
		{
			*proto = IPPROTO_IPV6;
			return qbuff_maclen(buff);
		}

		return -1;

	} break;
	case IPPROTO_IP: {

		struct iphdr _iph;
		const struct iphdr *ip;

		ip = qbuff_header_pointer(buff, offset, sizeof(_iph), &_iph);
		if (ip == NULL)
			return -1;

                return next_ip_offset(buff, offset + (ip->ihl<<2), ip->protocol, proto);

	} break;
	}

	return -1;
}


static inline const void *
qbuff_generic_ip_header_pointer(struct qbuff * buff, int ip_proto, int offset, int len, void *buffer)
{
	int ipoff = buff->monad->ipoff;

	if (unlikely(ipoff < 0))
		return NULL;

	if (buff->monad->ipproto == IPPROTO_NONE)
	{
		int n = 0;
		do
		{
			ipoff = qbuff_next_ip_offset(buff, ipoff, &buff->monad->ipproto);
			if (ipoff < 0) {
				buff->monad->ipproto = IPPROTO_NONE;
				buff->monad->ipoff = -1;
				return NULL;
			}
		}
		while (n++ < buff->monad->shift);

		buff->monad->ipoff = ipoff;
	}

	if (buff->monad->ipproto != ip_proto)
		return NULL;

	return qbuff_header_pointer(buff, buff->monad->ipoff + offset, len, buffer);
}

#define qbuff_ip_header_pointer(buff, offset, len, buffer)  qbuff_generic_ip_header_pointer(buff, IPPROTO_IP, offset, len, buffer)


static inline int
qbuff_ip_version(struct qbuff * buff)
{
	if (unlikely(buff->monad->ipoff < 0))
		return 0;

	if (buff->monad->ipproto == IPPROTO_NONE)
	{
		qbuff_ip_header_pointer(buff, 0, 0, NULL);
	}

	return buff->monad->ipproto == IPPROTO_IP   ? 4 :
	       buff->monad->ipproto == IPPROTO_IPV6 ? 6 : 0;
}


static inline int
qbuff_ip_protocol(struct qbuff * buff)
{
	switch(qbuff_ip_version(buff))
	{
	case 4: {
		struct iphdr _iph;
		const struct iphdr *ip;
		ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
		if (ip)
			return ip->protocol;
	} break;
	}

	return IPPROTO_NONE;
}


#endif /* PFQ_LANG_QBUFF_H */
