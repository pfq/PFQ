#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/init.h>

#include <linux/pf_q.h>
#include <linux/pf_q-fun.h>


MODULE_LICENSE("GPL");


/* Basic RTP header */
struct rtphdr {
	uint8_t  rh_flags;	/* T:2 P:1 X:1 CC:4 */
	uint8_t  rh_pt;		/* M:1 PT:7 */
	uint16_t rh_seqno;	/* sequence number */
	uint32_t rh_ts;		/* media-specific time stamp */
	uint32_t rh_ssrc;	/* synchronization src id */
	/* data sources follow per cc */
};

struct rtcphdr {
	uint8_t  rh_flags;	/* T:2 P:1 CNT:5 */
	uint8_t  rh_type;	/* type */
	uint16_t rh_len;	/* length of message (in bytes) */
	uint32_t rh_ssrc;	/* synchronization src id */
};

/* Rtp/Rtcp packet */

struct headers {
	struct udphdr udp;
	union
	{
		struct rtphdr 	rtp;
		struct rtcphdr  rtcp;
	} un;

} __attribute__((packed));


static inline
bool valid_codec(uint8_t c)
{
	return c < 19  ? true :
	       c > 95  ? true :
	       c == 25 ? true :
	       c == 26 ? true :
	       c == 28 ? true :
	       c == 31 ? true :
	       c == 32 ? true :
	       c == 33 ? true :
	       c == 34 ? true : false;
}


ret_t
heuristic_rtp(struct sk_buff *skb, ret_t ret, bool steer)
{
	sk_function_t fun = get_next_function(skb);

        if (is_skip(ret) || is_drop(ret))
                return pfq_call(get_next_function(skb), skb, ret);

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct headers _hdr;
		const struct headers *hdr;

                uint16_t source,dest;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
        		return pfq_call(fun, skb, drop());

		if (ip->protocol != IPPROTO_UDP)
        		return pfq_call(fun, skb, drop());

		hdr = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_hdr), &_hdr);
		if (hdr == NULL)
        		return pfq_call(fun, skb, drop());

		/* version => 2 */

		if (!((ntohs(hdr->un.rtp.rh_flags) & 0xc000) == 0x8000))
        		return pfq_call(fun, skb, drop());

		dest   = ntohs(hdr->udp.dest);
		source = ntohs(hdr->udp.source);

		if (dest < 1024 || source < 1024)
        		return pfq_call(fun, skb, drop());

		if ((dest & 1) && (source & 1))  /* rtcp */
		{
                	if (hdr->un.rtcp.rh_type != 200)  /* SR  */
        			return pfq_call(fun, skb, drop());
		}
		else if (!((dest & 1) || (source & 1)))
		{
                	uint8_t pt = hdr->un.rtp.rh_pt;
                 	if (!valid_codec(pt))
        			return pfq_call(fun, skb, drop());
		}

		return steer ? steering(Q_CLASS_DEFAULT, ip->saddr ^ ip->daddr ^ ((uint32_t)(hdr->udp.source & 0xfffe) << 16) ^ (hdr->udp.dest & 0xfffe)) :
		               pfq_call(fun, skb, ret);

	}

        return pfq_call(fun, skb, drop());
}


ret_t
filter_rtp(struct sk_buff *skb, ret_t ret)
{
        return heuristic_rtp(skb, ret, false);
}

ret_t
steering_rtp(struct sk_buff *skb, ret_t ret)
{
        return heuristic_rtp(skb, ret, true);
}


struct sk_function_descr hooks[] = {
	{ "rtp",       filter_rtp },
	{ "steer-rtp", steering_rtp },
	{ NULL, NULL}};


static int __init usr_init_module(void)
{
	return pfq_register_functions("[RTP]", hooks);
}


static void __exit usr_exit_module(void)
{
	pfq_unregister_functions("[RTP]", hooks);
}


module_init(usr_init_module);
module_exit(usr_exit_module);


