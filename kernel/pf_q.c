/***************************************************************
 *                                                
 * (C) 2011 - Nicola Bonelli <nicola.bonelli@cnit.it>   
 *            Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 *
 ****************************************************************/

#include <linux/kernel.h>
#include <linux/version.h>
#include <linux/module.h>
#include <linux/moduleparam.h>
#include <linux/semaphore.h>
#include <linux/socket.h>  
#include <linux/types.h>
#include <linux/skbuff.h>
#include <linux/highmem.h>
#include <linux/ioctl.h>
#include <linux/ip.h>
#include <linux/poll.h>

#include <net/sock.h>
#ifdef CONFIG_INET
#include <net/inet_common.h>
#endif

#define __PFQ_MODULE__
#include <linux/pf_q.h>

#include <pf_q-priv.h>
#include <pf_q-devmap.h>
#include <mpdb-queue.h>

struct net_proto_family  pfq_family_ops;
struct packet_type       pfq_prot_hook;
struct proto             pfq_proto;
struct proto_ops         pfq_ops; 

static int direct_path  = 0;
static int pipeline_len = 64;
static int queue_mem    = 65536*128;
static int cap_len      = 1514;

DEFINE_SEMAPHORE(loadbalance_sem);

static unsigned long long loadbalance_mask = 0;

struct pfq_pipeline    pfq_skb_pipeline[Q_MAX_CPU];

MODULE_LICENSE("GPL");

MODULE_AUTHOR("Nicola Bonelli <nicola.bonelli@cnit.it>");
MODULE_AUTHOR("Andrea Di Pietro <andrea.dipietro@for.unipi.it>");

MODULE_DESCRIPTION("packet catpure system for 64bit multi-core architecture");

module_param(direct_path,  int, 0644);
module_param(pipeline_len, int, 0644);
module_param(queue_mem,    int, 0644);
module_param(cap_len,      int, 0644);


MODULE_PARM_DESC(direct_path, " Direct Path: 0 = off(classic), 1 = direct");
MODULE_PARM_DESC(cap_len,     " Default capture length (bytes)");
MODULE_PARM_DESC(pipeline_len," Pipeline length");
MODULE_PARM_DESC(queue_mem,   " Queue memory (bytes)");


/* atomic vector of pointers to pfq_opt */
atomic_long_t pfq_vector[Q_MAX_ID]; 


/* uhm okay, this is a legit form of static polymorphism */

static inline struct pfq_sock *
pfq_sk(struct sock *sk)
{
    return (struct pfq_sock *)(sk);
}

inline void *kmap_skb_frag(const skb_frag_t *frag)
{
#ifdef CONFIG_HIGHMEM
    BUG_ON(in_irq());

    local_bh_disable();
#endif
    return kmap_atomic(frag->page, KM_SKB_DATA_SOFTIRQ);
}

inline void kunmap_skb_frag(void *vaddr)
{
    kunmap_atomic(vaddr, KM_SKB_DATA_SOFTIRQ);
#ifdef CONFIG_HIGHMEM
    local_bh_enable();
#endif
}


inline 
int pfq_get_free_id(struct pfq_opt * pq)
{
    int n = 0;
    for(; n < Q_MAX_ID; n++)
    {            
        if (!atomic_long_cmpxchg(pfq_vector + n, 0, (long)pq))
            return n;         
    }
    return -1;
}


inline 
struct pfq_opt * 
pfq_get_opt(unsigned int id)
{
    if (unlikely(id >= Q_MAX_ID))
    {
        printk(KERN_WARNING "[PF_Q]: pfq_devmap_freeid: bad id(%u)\n", id);
        return 0;
    }
    return (struct pfq_opt *)pfq_vector[id].counter;  // atomic_read not required here.
}


inline 
void pfq_release_id(unsigned int id)
{
    if (unlikely(id >= Q_MAX_ID))
    {
        printk(KERN_WARNING "[PF_Q]: pfq_devmap_freeid: bad id(%u)\n", id);
        return;
    }
    atomic_long_set(pfq_vector + id, 0);
}


inline
bool pfq_filter(const struct sk_buff *skb)
{
    return true;
}


bool pfq_enqueue_skb(struct sk_buff *skb, struct pfq_opt *pq, bool clone)
{
    /* eventually filter the packet... */

    if (!pfq_filter(skb))
    {
        sparse_inc(&pq->q_stat.drop);
        return false;
    }



    /* enqueue the sk_buff: it's wait-free. */

    if ( pq->q_active && mpdb_enqueue(pq, skb) ) {

        /* increment recv counter */
        sparse_inc(&pq->q_stat.recv);
        return true;
    }
    else {
        sparse_inc(&pq->q_stat.lost);
        return false;
    }

    return true;
}


/* pfq load balancer */

unsigned long 
pfq_load_balancer(unsigned long bm, const struct sk_buff *skb)
{ 
    int index[sizeof(unsigned long)<<3], i = 0;
    unsigned long candidates = bm & loadbalance_mask;
    unsigned long nolb = bm ^ candidates;

    struct ethhdr * eh;
    struct iphdr * ip;

    uint16_t proto;
    uint32_t hash;

    int mac_len = sizeof(struct ethhdr);

    if (candidates == 0)
        return nolb;

    while(candidates)
    {
        int zn = __builtin_ctz(candidates);
        index[i++] = zn;
        candidates ^= (1<<zn);
    }

    eh = (struct ethhdr *)skb_mac_header(skb);
    proto = ntohs(eh->h_proto);

    if (proto == 0x8100) {  /* vlan */
        proto = *(uint16_t *)((char *)&eh->h_proto + 4); 
        mac_len += 4;    
    }

    if (proto != 0x800)
        return 0;

    ip = (struct iphdr *)((unsigned char *)skb_mac_header(skb)+mac_len);

    hash = ip->saddr ^ ip->daddr;
    return nolb | ( 1 << index[hash % i] );
}


/* pfq skb handler */


int pfq_direct_receive(struct sk_buff *skb, int index, int queue)
{       
    struct pfq_opt * pq;
    unsigned long bm;
    int me = get_cpu();
    int q;

    /* if required, timestamp this packet now (early) */

    if (skb->tstamp.tv64 == 0) {
        __net_timestamp(skb);
    }

    if (!direct_path && skb_shared(skb)) {
        struct sk_buff *nskb = skb_clone(skb, GFP_ATOMIC);
        if (nskb == NULL) {
            skb = NULL;
        } 
        else {
            kfree_skb(skb);
            skb = nskb;
        }
    }

    pq = NULL; 

    /* get the clone/balancing bitmap */

    bm =  pfq_devmap_get(index, queue);

    /* load balancer among sockets */

    if (loadbalance_mask)
    {
        bm = pfq_load_balancer(bm, skb);
    }

    /* send this packet to eligible sockets */

    while ( bm != 0 )
    {        
        unsigned long lsb = bm & -bm;
        unsigned int zn = __builtin_ctz(lsb);
        struct pfq_opt * pq = pfq_get_opt(zn);

        bm &= ~lsb;

        if (pq == NULL)
            continue;

        pfq_enqueue_skb(skb, pq, bm != 0);
    }

    ////////////////////////////////////////////////////////////

    if (pfq_skb_pipeline[me].counter < pipeline_len-1) {

        pfq_skb_pipeline[me].queue[
            pfq_skb_pipeline[me].counter++
        ] = skb;

        return 0;
    }
    
    pfq_skb_pipeline[me].queue[pipeline_len-1] = skb;

    for(q = 0; q < pipeline_len; q++)
    {
        if(pfq_skb_pipeline[me].queue[q]) 
        {
            if (likely(direct_path))
                __kfree_skb(pfq_skb_pipeline[me].queue[q]);
            else
                kfree_skb(pfq_skb_pipeline[me].queue[q]);
        }
        
        __builtin_prefetch (&pfq_skb_pipeline[me].queue[q+1], 0, 1);
        __builtin_prefetch (&pfq_skb_pipeline[me].queue[q],   1, 1);
        pfq_skb_pipeline[me].queue[q]=0;
    }

    pfq_skb_pipeline[me].counter=0;
    return 0;
}

/* simple HANDLER */       

int pfq_packet_rcv
(
    struct sk_buff *skb, struct net_device *dev,
    struct packet_type *pt
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,16))
    ,struct net_device *orig_dev
#endif
    )
{
    return pfq_direct_receive(skb, dev->ifindex, skb_get_rx_queue(skb));
}

static
int
pfq_queue_alloc(struct pfq_opt *pq)
{
    /* calculate the size of the buffer */
    
    int tot_mem = PAGE_ALIGN(sizeof(struct pfq_queue_descr) + pq->q_queue_mem * 2); // double-buffered

    /* align bufflen to page size */

    int num_pages = tot_mem / PAGE_SIZE;
    num_pages += (num_pages + (SHMLBA-1)) % SHMLBA;
    tot_mem = num_pages*PAGE_SIZE;

    /* Memory is already zeroed */
    pq->q_mem = vmalloc_user(tot_mem);

    if (pq->q_mem == NULL)
    {
        printk(KERN_INFO "pfq_queue_alloc: out of memory");
        return -1;
    }

    pq->q_tot_mem = tot_mem;
    return 0;
}


static void
pfq_queue_free(struct pfq_opt *pq)
{
    vfree(pq->q_mem);
    pq->q_mem = NULL;
}    


static int pfq_ctor(struct pfq_opt *pq)
{

#ifdef Q_DEBUG
    printk(KERN_INFO "[PF_Q] queue ctor\n");
#endif
    pq->q_active = false;

    /* initialize queue: nothing to do */


    /* get a unique id for this queue */
    pq->q_id = pfq_get_free_id(pq);
    if (pq->q_id == -1)
    {
        printk(KERN_WARNING "[PF_Q]: no queue available\n");
        return -EBUSY;
    }

    pq->q_queue_mem = queue_mem;
    pq->q_cap_len = cap_len;

    /* alloc memory */
    pfq_queue_alloc(pq);

    /* reset stats */
    sparse_set(0, &pq->q_stat.recv);
    sparse_set(0, &pq->q_stat.lost);
    sparse_set(0, &pq->q_stat.drop);

    /* initialize waitqueue */

    init_waitqueue_head(&pq->q_waitqueue);
    return 0;
}


static void pfq_dtor(struct pfq_opt *pq)
{
#ifdef Q_DEBUG
    printk(KERN_INFO "[PF_Q] queue dtor\n");
#endif
    pfq_release_id(pq->q_id); 

    /* clean the loadbalance bit */
    
    down(&loadbalance_sem);
    
    loadbalance_mask &= ~(1<< pq->q_id);

    up(&loadbalance_sem);

    pfq_queue_free(pq);
}


static int pfq_create(
#if(LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,24))
    struct net *net,
#endif
    struct socket *sock, int protocol
#if(LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,33))
    , int kern
#endif
    )
{
    struct pfq_opt *pq;
    struct sock *sk;
    struct pfq_sock *psk;
    int err = -ENOMEM;

    /* security and sanity check */
    if (!capable(CAP_NET_ADMIN))
        return -EPERM;
    if (sock->type != SOCK_RAW)
        return -ESOCKTNOSUPPORT;
    if (protocol != htons(ETH_P_ALL))
        return -EPROTONOSUPPORT;

#if(LINUX_VERSION_CODE <= KERNEL_VERSION(2,6,11))
    sk = sk_alloc(PF_Q, GFP_KERNEL, 1, NULL);
#else
#if(LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24))
    // BD: API changed in 2.6.12, ref:
    // http://svn.clkao.org/svnweb/linux/revision/?rev=28201
    sk = sk_alloc(PF_Q, GFP_ATOMIC, &q_proto, 1);
#else
    sk = sk_alloc(net, PF_INET, GFP_KERNEL, &pfq_proto);
#endif
#endif
    if (sk == NULL)
        goto out;

    sock->ops = &pfq_ops;
    sock_init_data(sock,sk);

#if(LINUX_VERSION_CODE <= KERNEL_VERSION(2,6,11))
    sk_set_owner(sk, THIS_MODULE);
#endif
    /* alloc memory for this pq */

    pq = (struct pfq_opt *)kmalloc( sizeof(struct pfq_opt), GFP_KERNEL);
    if (!pq) 
    {
        err = -ENOMEM;
        goto pq_err;
    }   

    /* construct pfq_opt */
    if (pfq_ctor(pq) != 0)
    {
        err = -ENOMEM;
        goto ctor_err;
    }

    /* store the pq */
    psk = pfq_sk(sk);
    psk->opt = pq;
    return 0;

ctor_err:    
    kfree(pq);
pq_err:    
    sk_free(sk);
out:    
    return err;
}   

static int pfq_release(struct socket *sock)
{
    struct sock * sk = sock->sk;
    struct pfq_opt * pq = pfq_sk(sk)->opt;
    int sched_time = 0;

    if(!pq)
        return 0;

    /* remove this pq from demux matrix */
    pfq_devmap_update(map_reset, Q_ANY_DEVICE, Q_ANY_QUEUE, pq->q_id);

    pq->q_active = false;
    wmb();

    /* TODO: reschedule for a while... */
    while(sched_time++ < 64)
        schedule();

    sock_orphan(sk);
    sock_put(sk);

    sock->sk = NULL;

    pfq_dtor(pq);
    kfree(pq);

#ifdef Q_DEBUG
    printk(KERN_INFO "[PF_Q] queue freed.\n");
#endif
    return 0;
}


static
int pfq_getsockopt(struct socket *sock,
                   int level, int optname,
                   char __user * optval, int __user * optlen)
{
    int len;
    struct pfq_opt *pq = pfq_sk(sock->sk)->opt;

    if (pq == NULL)
        return -EINVAL;
    if (get_user(len, optlen) || len < 0)
        return -EFAULT;

    switch(optname)
    {
    case SO_GET_ID: 
        {
            if (len != sizeof(int))
                return -EINVAL;
            if (copy_to_user(optval, &pq->q_id, sizeof(pq->q_id)))
                return -EFAULT;
        } break;

    case SO_GET_QUEUE_MEM: 
        {
            if (len != sizeof(pq->q_queue_mem))
                return -EINVAL;
            if (copy_to_user(optval, &pq->q_queue_mem, sizeof(pq->q_queue_mem)))
                return -EFAULT;
        } break;

    case SO_GET_TOT_MEM: 
        {
            if (len != sizeof(int))
                return -EINVAL;
            if (copy_to_user(optval, &pq->q_tot_mem, sizeof(pq->q_tot_mem)))
                return -EFAULT;
        } break;

    case SO_GET_TSTAMP_TYPE: 
        {
            if (len != sizeof(int))
                return -EINVAL;
            if (copy_to_user(optval, &pq->q_tstamp_type, sizeof(pq->q_tstamp_type)))
                return -EFAULT;
        } break;

    case SO_GET_STATUS: 
        {
            if (len != sizeof(int))
                return -EINVAL;
            if (copy_to_user(optval, &pq->q_active, sizeof(pq->q_active)))
                return -EFAULT;
        } break;

    case SO_GET_OWNERS: 
        {
            struct pfq_dev_queue dq;
            unsigned long owners; 

            if (len != sizeof(struct pfq_dev_queue))
                return -EINVAL;
            if (copy_from_user(&dq, optval, len))
                return -EFAULT;

            owners = pfq_devmap_get(dq.if_index, dq.hw_queue);

            if (copy_to_user(optval, &owners, sizeof(long)))
                return -EFAULT;
        } break;

    case SO_GET_STATS: 
        {
            struct pfq_stats stat;
            if (len != sizeof(struct pfq_stats))
                return -EINVAL;

            stat.recv = sparse_read(&pq->q_stat.recv);
            stat.lost = sparse_read(&pq->q_stat.lost);
            stat.drop = sparse_read(&pq->q_stat.drop);

            if (copy_to_user(optval, &stat, sizeof(stat)))
                return -EFAULT;
        } break;

    default:
        return -EFAULT;
    }

    return 0;
}

static
int pfq_setsockopt(struct socket *sock,
                   int level, int optname,
                   char __user * optval,
#if(LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,31))
                   unsigned
#endif
                   int optlen)
{
    struct pfq_opt *pq = pfq_sk(sock->sk)->opt;
    long int val;
    bool found = true;

    if (pq == NULL)
        return -EINVAL;
    if (get_user(val, (long int *)optval))
        return -EFAULT;

    switch(optname)
    {

    case SO_TOGGLE_QUEUE: 
        {
            if (optlen != sizeof(pq->q_active))
                return -EINVAL;
            if (copy_from_user(&pq->q_active, optval, optlen))
                return -EFAULT;
        } break;

    case SO_LOAD_BALANCE: 
        {
            int value;
            if (optlen != sizeof(value))
                return -EINVAL;
            if (copy_from_user(&value, optval, optlen))
                return -EFAULT;
            
            if (down_interruptible(&loadbalance_sem) != 0)
                return -EINTR;
            
            if (value)
                loadbalance_mask |= (1 << pq->q_id);
            else
                loadbalance_mask &= ~(1 << pq->q_id);
            
            up(&loadbalance_sem);
        } break;

    case SO_ADD_DEVICE: 
        {
            struct pfq_dev_queue dq;
            if (optlen != sizeof(struct pfq_dev_queue)) {
                return -EINVAL;
            }
            if (copy_from_user(&dq, optval, optlen))
                return -EFAULT;

            pfq_devmap_update(map_set, dq.if_index, dq.hw_queue, pq->q_id);
        } break;

    case SO_REMOVE_DEVICE: 
        {
            struct pfq_dev_queue dq;
            if (optlen != sizeof(struct pfq_dev_queue))
                return -EINVAL;
            if (copy_from_user(&dq, optval, optlen))
                return -EFAULT;
            pfq_devmap_update(map_reset, dq.if_index, dq.hw_queue, pq->q_id);
        } break;

    case SO_TSTAMP_TYPE: 
        {
            if (optlen != sizeof(pq->q_tstamp_type))
                return -EINVAL;
            if (copy_from_user(&pq->q_tstamp_type, optval, optlen))
                return -EFAULT;
        } break;
    
    default: 
        {
            found = false; 
        } break;
    }

    return found ? 0 : sock_setsockopt(sock, level, optname, optval, optlen);
}        


static
int
pfq_memory_mmap(struct vm_area_struct *vma,
                unsigned long size, char *ptr, unsigned int flags)
{
    unsigned long start;

    vma->vm_flags |= flags;

    start = vma->vm_start;
    if (remap_vmalloc_range(vma, ptr, 0) != 0)
    {
        printk(KERN_INFO "pfq: remap_vmalloc_range\n");
        return -EAGAIN;
    }

    return 0;
}

static int pfq_mmap(struct file *file,
                    struct socket *sock, struct vm_area_struct *vma)
{
    struct pfq_opt *pq = pfq_sk(sock->sk)->opt;
    unsigned long size = (unsigned long)(vma->vm_end - vma->vm_start);
    int ret;

    if(size % PAGE_SIZE) {
        printk(KERN_INFO "pfq_mmap: len not multiple of PAGE_SIZE\n");
        return -EINVAL;
    }

    if(size > pq->q_tot_mem) {
        printk(KERN_INFO "pfq_mmap: area too large\n");
        return -EINVAL;
    }

    if((ret = pfq_memory_mmap(vma, size, pq->q_mem, VM_LOCKED)) < 0)
        return ret;

    return 0;
}

unsigned int pfq_poll(struct file *file, struct socket *sock, poll_table * wait)
{
    struct pfq_opt *pq = pfq_sk(sock->sk)->opt;
    struct pfq_queue_descr *q = (struct pfq_queue_descr *)pq->q_mem;
    
    unsigned int mask = 0;

    if (mpdb_queue_size(pq) >= (pq->q_queue_mem >> 1)) {
        q->poll_wait = 0; 
        mask |= POLLIN | POLLRDNORM;
    }
    else if (!q->poll_wait) {
        q->poll_wait = 1;
        poll_wait(file, &pq->q_waitqueue, wait);
    }

    return mask;
}


static
int 
pfq_ioctl(struct socket *sock, unsigned int cmd, unsigned long arg)
{
    switch (cmd) {
#ifdef CONFIG_INET
    case SIOCGIFFLAGS:
    case SIOCSIFFLAGS:
    case SIOCGIFCONF:
    case SIOCGIFMETRIC:
    case SIOCSIFMETRIC:
    case SIOCGIFMEM:
    case SIOCSIFMEM:
    case SIOCGIFMTU:
    case SIOCSIFMTU:
    case SIOCSIFLINK:
    case SIOCGIFHWADDR:
    case SIOCSIFHWADDR:
    case SIOCSIFMAP:
    case SIOCGIFMAP:
    case SIOCSIFSLAVE:
    case SIOCGIFSLAVE:
    case SIOCGIFINDEX:
    case SIOCGIFNAME:
    case SIOCGIFCOUNT:
    case SIOCSIFHWBROADCAST:
        return(inet_dgram_ops.ioctl(sock, cmd, arg));
#endif
    default:
        return -ENOIOCTLCMD;
    }

    return 0;
}


static
void pfq_proto_ops_ctor(void)
{
    pfq_ops = (struct proto_ops) 
    {
        .family = PF_Q,
        .owner = THIS_MODULE,

        /* Operations that make no sense on queue sockets. */
        .connect    = sock_no_connect,
        .socketpair = sock_no_socketpair,
        .accept     = sock_no_accept,
        .getname    = sock_no_getname,
        .listen     = sock_no_listen,
        .shutdown   = sock_no_shutdown,
        .sendpage   = sock_no_sendpage,

        /* Now the operations that really occur. */
        .release    = pfq_release,    
        .bind       = sock_no_bind,         // pfq_bind,
        .mmap       = pfq_mmap,             // pfq_mmap,
        .poll       = pfq_poll,             // pfq_poll,
        .setsockopt = pfq_setsockopt,       // pfq_setsockopt,
        .getsockopt = pfq_getsockopt,       // pfq_getsockopt,
        .ioctl      = pfq_ioctl,            // pfq_ioctl,
        .recvmsg    = sock_no_recvmsg,      // pfq_recvmsg,
        .sendmsg    = sock_no_sendmsg       // pfq_sendmsg,
    };
}


static
void pfq_proto_ctor(void)
{
    pfq_proto = (struct proto)
    {
        .name  = "PF_Q",
        .owner = THIS_MODULE,
        .obj_size = sizeof(struct pfq_sock)
    };
}


static
void pfq_net_proto_family_ctor(void)
{
    pfq_family_ops = (struct net_proto_family)
    {
        .family = PF_Q,
        .create = pfq_create,
        .owner = THIS_MODULE,
    };
} 


static
void register_device_handler(void)
{
    if (direct_path)
        return;
    pfq_prot_hook.func = pfq_packet_rcv;
    pfq_prot_hook.type = htons(ETH_P_ALL);
    dev_add_pack(&pfq_prot_hook);
}


static
void unregister_device_handler(void) 
{
    if (direct_path)
        return;
    dev_remove_pack(&pfq_prot_hook); /* Remove protocol hook */
}


static int __init pfq_init_module(void)
{
    int n;
    printk(KERN_WARNING "[PF_Q] loaded (%s)\n", Q_VERSION);

    pfq_net_proto_family_ctor();
    pfq_proto_ops_ctor();
    pfq_proto_ctor();

    /* register pfq sniffer protocol */    
    n = proto_register(&pfq_proto, 0);
    if (n != 0)
        return n;

    /* register the pfq socket */
    sock_register(&pfq_family_ops);

    /* finally register the basic device handler */
    register_device_handler();

    return 0;
}


static void __exit pfq_exit_module(void)
{        
    int i,n;

    /* unregister the basic device handler */
    unregister_device_handler();

    /* unregister the pfq socket */
    sock_unregister(PF_Q);

    /* unregister the pfq protocol */
    proto_unregister(&pfq_proto);

    /* destroy pipeline queues */
    for(n=0; n < Q_MAX_CPU; n++) {
        for(i=0 ; i< PFQ_PIPELINE_MAX_LEN; i++) {
            kfree_skb(pfq_skb_pipeline[n].queue[i]);
            pfq_skb_pipeline[n].queue[i] = NULL;
        }
    }

    printk(KERN_WARNING "[PF_Q] unloaded\n");
}


/* pfq-10 aware drivers support */


int pfq_direct_capture(const struct sk_buff *skb)
{
    return direct_path;
}


const char *
pfq_version(void)
{
    return Q_VERSION;
}


EXPORT_SYMBOL_GPL(pfq_direct_capture);
EXPORT_SYMBOL_GPL(pfq_direct_receive);
EXPORT_SYMBOL_GPL(pfq_version);


module_init(pfq_init_module);
module_exit(pfq_exit_module);
