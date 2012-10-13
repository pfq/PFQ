#include <stdio.h>
#include <stdlib.h>

#include <pfq.h>

#define MIN(a,b) (a < b ? a : b)

int
main(int argc, char *argv[])
{
	int n;
        if (argc < 2) {
                fprintf(stderr, "usage: %s dev\n", argv[0]);
                return 0;
        }

        pfq_t *p = pfq_open(64, 0, 4096);
        if (p == NULL) {
                printf("error: %s\n", pfq_error(p));
                return -1;
        }

        if (pfq_enable(p) < 0) {
                printf("error: %s\n", pfq_error(p));
                return -1;
        }

        if (pfq_bind(p, argv[1], Q_ANY_QUEUE) < 0) {
        	printf("error: %s\n", pfq_error(p));
        	return -1;
        }

        if (pfq_set_timestamp(p, 1) < 0) {
        	printf("error: %s\n", pfq_error(p));
        	return -1;
	}

	printf("reading from %s...\n", argv[1]);
        
	
	for(n = 0; n < 100; n++) {

                struct pfq_net_queue nq;
		pfq_iterator_t it, it_e;
		
		int many = pfq_read(p, &nq, 1000000);
		if (many < 0) {
                	printf("error: %s\n", pfq_error(p));
			break;
		}
                 
		printf("queue size: %zd\n", nq.len);

		it = pfq_net_queue_begin(&nq);
		it_e = pfq_net_queue_end(&nq);

		for(; it != it_e; it = pfq_net_queue_next(&nq, it))
		{
			int x;

			while (!pfq_iterator_ready(&nq, it))
				pfq_yield();

			const struct pfq_hdr *h = pfq_iterator_header(it);

                    	printf("caplen:%d len:%d ifindex:%d hw_queue:%d tstamp: %u:%u -> ", 
					h->caplen, h->len, h->if_index, h->hw_queue,
                                        h->tstamp.tv.sec, h->tstamp.tv.nsec);
                    	
			const char *buff = pfq_iterator_data(it); 

			for(x=0; x < MIN(h->caplen, 34); x++)
			{
				printf("%2x ", (unsigned char)buff[x]);
			}
			printf("\n");
		}
        }

        // struct pfq_stats s = pfq_get_stats(p, &ok);
        // if (!ok) {
        //         printf("error: %s\n", pfq_error(p));
        //         return -1;
        // }

        // printf("stats:: recv=%d lost=%d drop=%d\n", s.recv, s.lost, s.drop);

        pfq_close(p);
        return 0;
}

