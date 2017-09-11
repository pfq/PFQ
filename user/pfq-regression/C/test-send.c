#include <stdio.h>
#include <stdlib.h>

#include <pfq/pfq.h>

/* Frame (98 bytes) */

static const unsigned char ping[98] =
{
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xf0, 0xbf, /* L`..UF.. */
        0x97, 0xe2, 0xff, 0xae, 0x08, 0x00, 0x45, 0x00, /* ......E. */
        0x00, 0x54, 0xb3, 0xf9, 0x40, 0x00, 0x40, 0x01, /* .T..@.@. */
        0xf5, 0x32, 0xc0, 0xa8, 0x00, 0x02, 0xad, 0xc2, /* .2...... */
        0x23, 0x10, 0x08, 0x00, 0xf2, 0xea, 0x42, 0x04, /* #.....B. */
        0x00, 0x01, 0xfe, 0xeb, 0xfc, 0x52, 0x00, 0x00, /* .....R.. */
        0x00, 0x00, 0x06, 0xfe, 0x02, 0x00, 0x00, 0x00, /* ........ */
        0x00, 0x00, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, /* ........ */
        0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, /* ........ */
        0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, /* .. !"#$% */
        0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, /* &'()*+,- */
        0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, /* ./012345 */
        0x36, 0x37                                      /* 67 */
};


void send_packets(pfq_t *q, unsigned long long num)
{
        unsigned long long n;

	printf("sending %llu packets:\n", num);

        for(n = 0; n < num;)
        {
                if (pfq_send(q, ping, sizeof(ping), 1, 1)) {
			n++;
		}
        }
}


void send_packets_async(pfq_t *q, unsigned long long num)
{
        unsigned long long n;

	printf("sending %llu packets (async):\n", num);
        for(n = 0; n < num;)
        {
                if (pfq_send_async(q, ping, sizeof(ping), 1, 1) != -1) {
			n++;
		}
        }
}


int
main(int argc, char *argv[])
{
        if (argc < 5)
        {
                fprintf(stderr, "usage: %s dev queue kthread num\n", argv[0]);
                return -1;
        }

        const char *dev = argv[1];
        int queue  = atoi(argv[2]);
        int kthread = atoi(argv[3]);
        unsigned long long num = atoll(argv[4]);

        pfq_t * q= pfq_open(64, 1024, 64, 1024);

        if (pfq_bind_tx(q, dev, queue, kthread) < 0) {
		fprintf(stderr, "%s\n", pfq_error(q));
		return -1;
	}

        pfq_enable(q);

	if (kthread == -1) {
		send_packets(q, num);
	}
	else  {
		send_packets_async(q, num);
	}

        sleep(2);

        struct pfq_stats stat;
        pfq_get_stats(q, &stat);

        fprintf(stdout, "sent: %lu - disc: %lu\n", stat.sent, stat.disc);

        pfq_close(q);

        return 0;
}

