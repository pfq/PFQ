#include <stdio.h>
#include <stdlib.h>

#include <pfq/pfq.h>

#define MIN(a,b) (a < b ? a : b)

void dispatch(char *user __attribute__((unused)), const struct pfq_pkthdr *h, const char *data)
{
        int x;
        for(x = 0; x < MIN(h->caplen,34); x++)
        {
                printf("%2x ", (unsigned char)data[x]);
        }
        printf("\n");
}

int
main(int argc, char *argv[])
{
        if (argc < 2) {
                fprintf(stderr, "usage: %s dev\n", argv[0]);
                return 0;
        }

        pfq_t *p = pfq_open(64, 4096, 64, 1024);
        if (p == NULL) {
                printf("error: %s\n", pfq_error(p));
                return -1;
        }

        if (pfq_enable(p) < 0) {
                printf("error: %s\n", pfq_error(p));
                return -1;
        }

        if (pfq_is_enabled(p) != 1) {
                printf("error: %s\n", pfq_error(p));
                return -1;
        }

        int caplen = pfq_get_caplen(p);
        if (caplen < 0) {
		printf("error: %s\n", pfq_error(p));
		return -1;
        }

        printf("caplen: %d\n", caplen);

        int id = pfq_id(p);
        if (id < 0) {
		printf("error: %s\n", pfq_error(p));
		return -1;
        }

        printf("id: %d\n", id);

        if (pfq_bind(p, argv[1], Q_ANY_QUEUE) < 0) {
		printf("error: %s\n", pfq_error(p));
		return -1;
        }

        int n = 0;
	printf("dispatching...\n");

	for(;n < 10; n++) {
                int many = pfq_dispatch(p, dispatch, 1000000, NULL);
                if (many < 0) {
                        printf("error: %s\n", pfq_error(p));
			break;
                }
		printf("queue length: %d\n", many);
        }

        struct pfq_stats s;
	if(pfq_get_stats(p, &s) < 0) {
                printf("error: %s\n", pfq_error(p));
                return -1;
        }

        printf("stats:: recv=%lu lost=%lu drop=%lu\n", s.recv, s.lost, s.drop);

	pfq_close(p);
        return 0;
}

