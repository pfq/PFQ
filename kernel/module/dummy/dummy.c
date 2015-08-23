#include <linux/kernel.h>
#include <linux/module.h>


static int __init pfq_init_module(void)
{
	return 0;
}

static void __exit pfq_exit_module(void)
{
}


module_init(pfq_init_module);
module_exit(pfq_exit_module);

