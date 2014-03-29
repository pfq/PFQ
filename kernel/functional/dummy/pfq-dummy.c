#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/init.h>

#include <linux/pf_q.h>
#include <linux/pf_q-module.h>

MODULE_LICENSE("GPL");

struct sk_buff *
fun_dummy(context_t ctx, struct sk_buff *skb)
{
	/* perform action here */
        return skb;
}


struct pfq_function_descr hooks[] = {

	{ "dummy", fun_dummy },
	{ NULL, NULL}
};



static int __init usr_init_module(void)
{
	return pfq_register_functions("[dummy]", &pfq_monadic_cat, hooks);
}


static void __exit usr_exit_module(void)
{
	pfq_unregister_functions("[dummy]", &pfq_monadic_cat, hooks);
}


module_init(usr_init_module);
module_exit(usr_exit_module);


