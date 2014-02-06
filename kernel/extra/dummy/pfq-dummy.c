#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/init.h>

#include <linux/pf_q.h>
#include <linux/pf_q-fun.h>

MODULE_LICENSE("GPL");

ret_t
steering_dummy(struct sk_buff *skb, ret_t ret)
{
	sk_function_t fun = get_next_function(skb);

        if (is_skip(ret) || is_drop(ret))
                return pfq_call(fun, skb, ret);

	/* perform action here */

        return pfq_call(fun, skb, ret);
}


struct sk_function_descr hooks[] = {
	{ "steer-dummy", steering_dummy },
	{ NULL, NULL}};



static int __init usr_init_module(void)
{
	return pfq_register_functions("[dummy]", hooks);
}


static void __exit usr_exit_module(void)
{
	pfq_unregister_functions("[dummy]", hooks);
}


module_init(usr_init_module);
module_exit(usr_exit_module);


