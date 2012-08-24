#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/init.h>

#include <linux/pf_q.h>
#include <linux/pf_q-steering.h>
 
MODULE_LICENSE("GPL");

steering_ret_t
steering_dummy(const struct sk_buff *skb, const void *state)
{
	const int * s = (const int *)state;
	if (state == NULL) {
		printk(KERN_INFO "[dummy] steering dummy: state => NULL\n");
		return none();
	}

	printk(KERN_INFO "[dummy] steering dummy: state = %d\n", *s);
	return clone(Q_CLASS_DEFAULT); 
}


struct steering_function hooks[] = {
	{ "steer-dummy", steering_dummy },
	{ NULL, NULL}};



static int __init usr_init_module(void)
{
	return pfq_register_steering_functions("[dummy]", hooks);
}


static void __exit usr_exit_module(void)
{                 
	pfq_unregister_steering_functions("[dummy]", hooks);
}
	

module_init(usr_init_module);
module_exit(usr_exit_module);


