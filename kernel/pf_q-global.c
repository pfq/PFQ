#include <pf_q-global.h>

MODULE_LICENSE("GPL");

DEFINE_SEMAPHORE(global_sem);

volatile struct pfq_global_t global;


