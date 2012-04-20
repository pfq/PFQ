#include <stdio.h>
#include <stdlib.h>

#include <pfq.h>

#define MIN(a,b) (a < b ? a : b)

void dispatch(char *user, const struct pfq_hdr *h, const char *data)
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
        int ok = 0;

        if (argc < 2) {
                fprintf(stderr, "usage: %s dev\n", argv[0]);
                return 0;
        }

        pfq_t *p = pfq_open(64, 0, 4096);
        if (p == NULL) {
                printf("error: %s\n", pfq_error(p));
                return -1;
        }

        pfq_enable(p, &ok);
        if (!ok) {
                printf("error: %s\n", pfq_error(p));
                return -1;
        }

        int enabled = pfq_is_enabled(p, &ok);
        if (!ok) {
                printf("error: %s\n", pfq_error(p));
                return -1;
        }

        int caplen = pfq_get_caplen(p, &ok);
        if (!ok) {
                printf("error: %s\n", pfq_error(p));
                return -1;
        }

        printf("caplen: %d\n", caplen);

        int offset = pfq_get_offset(p, &ok);
        if (!ok) {
                printf("error: %s\n", pfq_error(p));
                return -1;
        }

        printf("offset: %d\n", offset);

        int id = pfq_id(p, &ok);
        if (!ok) {
                printf("error: %s\n", pfq_error(p));
                return -1;
        }

        printf("id: %d fd-> %d\n", id, pfq_fd(p));

        pfq_add_device_by_name(p, argv[1], -1, &ok);
        if (!ok) {
                printf("error: %s\n", pfq_error(p));
                return -1;
        }

        int n = 0;
        for(;n < 100; n++) {
                int n = pfq_dispatch(p, dispatch, NULL, &ok);
                if (!ok) {
                        printf("error: %s\n", pfq_error(p));
                }
        }

        struct pfq_stats s = pfq_get_stats(p, &ok);
        if (!ok) {
                printf("error: %s\n", pfq_error(p));
                return -1;
        }

        printf("stats:: recv=%d lost=%d drop=%d\n", s.recv, s.lost, s.drop);

        pfq_close(p);
        return 0;
}

