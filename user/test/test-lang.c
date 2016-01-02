#include <stdio.h>
#include <stdlib.h>

#include <pfq/pfq.h>

#define MIN(a,b) (a < b ? a : b)

int
main(int argc, char *argv[])
{
        pfq_t *q = pfq_open(64, 4096, 1024);
        if (q == NULL) {
                printf("error: %s\n", pfq_error(q));
                return -1;
        }

        if (pfq_enable(q) < 0) {
                printf("error: %s\n", pfq_error(q));
                return -1;
        }

	printf("testing computation...\n");
	if (pfq_set_group_computation_from_string(q, pfq_group_id(q),
			"main = dummy 42 >-> dummy_ip \"192.168.0.1\" >-> dummy_string \"hello world!\" >-> dummy_vector [1,2,3] >-> bloom_filter 1024 [\"192.168.0.1\", \"10.10.10.10\"] 24 ") < 0) {
                printf("error: %s\n", pfq_error(q));
                return -1;
	}

	sleep(30);

        pfq_close(q);
        return 0;
}

