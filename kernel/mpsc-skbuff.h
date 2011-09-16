/***************************************************************
 *                                                
 * (C) 2011 - Nicola Bonelli <nicola.bonelli@cnit.it>   
 *            Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 *
 ****************************************************************/

#ifndef _MPSC_SKBUFF_H_
#define _MPSC_SKBUFF_H_ 

#include <linux/kernel.h>
#include <linux/skbuff.h>

typedef struct mpsc_queue
{
    atomic_long_t       head;
    struct sk_buff *    tail;

} mpsc_queue_t;


static inline 
int mpsc_queue_ctor(mpsc_queue_t * self)
{
    struct sk_buff * stub = dev_alloc_skb(1);
    if (stub != NULL)
    {
        stub->next = 0;
        atomic_long_set(&self->head, (long)stub);
        self->tail = stub;
    }
    else {
        printk(KERN_WARNING "mpsc_skbuff_queue_ctor: memory problem!\n");
        return -ENOMEM;
    }   
    return 0;
};

static inline 
void mpsc_queue_dtor(mpsc_queue_t *self)
{
    struct sk_buff * skb = self->tail;
    while(skb != NULL)
    {
        struct sk_buff * next = skb->next;
        kfree_skb(skb);
        skb = next;
    } 

    atomic_long_set(&self->head, (long)0);
    self->tail = (struct sk_buff *)0;  
}


static inline 
void mpsc_queue_push(mpsc_queue_t *self, struct sk_buff *skb)
{
    struct sk_buff *prev;

    skb->next = 0;
    prev = (struct sk_buff *) atomic_long_xchg(&self->head, (long)skb);
    prev->next = skb;
    
}

static inline 
struct sk_buff * mpsc_queue_pop(mpsc_queue_t *self)
{
    struct sk_buff * tail = self->tail;
    struct sk_buff * next = tail->next;

    if (next)
    {
        self->tail = next;
        tail->prev = next;   // data in this case is the skb itself: tail->data = next->data;
        return tail;
    }

    return NULL;
}

static inline 
const struct sk_buff * mpsc_queue_next(mpsc_queue_t *self)
{
    if (self->tail)
        return self->tail->next;
    return NULL;
}


#endif /* _MPSC_SKBUFF_H_ */
