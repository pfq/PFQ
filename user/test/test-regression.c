#include <sys/types.h>
#include <sys/wait.h>

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <pfq.h>

#include <pthread.h>


void test_enable_disable()
{
	pfq_t * q = pfq_open(64, 0, 1024);

	assert(q);
	assert(pfq_mem_addr(q) == NULL);
	assert(pfq_enable(q) == 0);
	assert(pfq_mem_addr(q) != NULL);
	assert(pfq_disable(q) == 0);
	assert(pfq_mem_addr(q) == NULL);

	pfq_close(q);
}


void test_is_enabled()
{
	pfq_t * q = pfq_open(64, 0, 1024);

	assert(q);
	assert(pfq_is_enabled(q) == 0);
	assert(pfq_enable(q) == 0);
	assert(pfq_is_enabled(q) == 1);
	assert(pfq_disable(q) == 0);
	assert(pfq_is_enabled(q) == 0);

	pfq_close(q);
}


void test_ifindex()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);
	assert(pfq_ifindex(q, "lo") != -1);
	pfq_close(q);
}


void test_timestamp()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);

	assert(pfq_is_timestamp_enabled(q) == 0);
	assert(pfq_timestamp_enable(q, 1) == 0);
	assert(pfq_is_timestamp_enabled(q) == 1);
	assert(pfq_timestamp_enable(q, 0) == 0);
	assert(pfq_is_timestamp_enabled(q) == 0);

	pfq_close(q);
}


void test_caplen()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);

	assert(pfq_get_caplen(q) == 64);
	assert(pfq_set_caplen(q, 128) == 0);
	assert(pfq_get_caplen(q) == 128);

	assert(pfq_enable(q) == 0);
	assert(pfq_set_caplen(q, 10) == -1);
	assert(pfq_disable(q) == 0);

	assert(pfq_set_caplen(q, 64) == 0);
	assert(pfq_get_caplen(q) == 64);

	pfq_close(q);
}


void test_maxlen()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);

	assert(pfq_get_maxlen(q) == 64);
	assert(pfq_set_maxlen(q, 128) == 0);
	assert(pfq_get_maxlen(q) == 128);

	assert(pfq_enable(q) == 0);
	assert(pfq_set_maxlen(q, 10) == -1);
	assert(pfq_disable(q) == 0);

	assert(pfq_set_maxlen(q, 64) == 0);
	assert(pfq_get_maxlen(q) == 64);

	pfq_close(q);
}

void test_offset()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);

    	assert(pfq_set_offset(q, 14) == 0);
       	assert(pfq_get_offset(q) == 14);

	assert(pfq_enable(q) == 0);
	assert(pfq_set_offset(q, 10) == -1);
	assert(pfq_disable(q) == 0);

       	assert(pfq_get_offset(q) == 14);

	pfq_close(q);
}

void test_rx_slots()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);

	assert(pfq_get_rx_slots(q) == 1024);

	assert(pfq_enable(q) == 0);
	assert(pfq_set_rx_slots(q, 4096) == -1);
	assert(pfq_disable(q) == 0);

	assert(pfq_set_rx_slots(q, 4096) == 0);
	assert(pfq_get_rx_slots(q) == 4096);

	pfq_close(q);
}


void test_tx_slots()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);

	assert(pfq_get_tx_slots(q) == 1024);

	assert(pfq_enable(q) == 0);
	assert(pfq_set_tx_slots(q, 4096) == -1);
	assert(pfq_disable(q) == 0);

	assert(pfq_set_tx_slots(q, 4096) == 0);
	assert(pfq_get_tx_slots(q) == 4096);

	pfq_close(q);
}


void test_rx_slot_size()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);
        size_t size = 64 + sizeof(struct pfq_pkt_hdr);
	assert(pfq_get_rx_slot_size(q) == (size + (size % 8)));
	pfq_close(q);
}


void test_bind_device()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);

       	assert(pfq_bind(q, "unknown", Q_ANY_QUEUE) == -1);
       	assert(pfq_bind(q, "eth0", Q_ANY_QUEUE) == 0);
	assert(pfq_bind_group(q, 11, "eth0", Q_ANY_QUEUE) == -1);

	pfq_close(q);
}


void test_unbind_device()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);

       	assert(pfq_unbind(q, "unknown", Q_ANY_QUEUE) == -1);
       	assert(pfq_unbind(q, "eth0", Q_ANY_QUEUE) == 0);

	assert(pfq_bind(q, "eth0", Q_ANY_QUEUE) == 0);
       	assert(pfq_unbind(q, "eth0", Q_ANY_QUEUE) == 0);

	assert(pfq_unbind_group(q, 11, "eth0", Q_ANY_QUEUE) == -1);

	pfq_close(q);
}

void test_poll()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);

	assert(pfq_poll(q, 0) == 0);

	pfq_close(q);
}


void test_read()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);

	struct pfq_net_queue nq;
        assert(pfq_read(q, &nq, 10) == -1);

	assert(pfq_enable(q) == 0);

        assert(pfq_read(q, &nq, 10) == 0);

	pfq_close(q);
}


void test_stats()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);

	struct pfq_stats s;
	assert(pfq_get_stats(q, &s) == 0);

	assert(s.recv == 0);
	assert(s.lost == 0);
	assert(s.drop == 0);

	pfq_close(q);
}


void test_group_stats()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);

	struct pfq_stats s;
	assert(pfq_get_group_stats(q, 11, &s) == 0);

	assert(pfq_join_group(q, 11, Q_CLASS_DEFAULT, Q_GROUP_RESTRICTED) == 11);

	assert(pfq_get_group_stats(q, 11, &s) == 0);

	assert(s.recv == 0);
	assert(s.lost == 0);
	assert(s.drop == 0);

	pfq_close(q);
}


void test_my_group_stats_priv()
{
	pfq_t * q = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_PRIVATE, 64, 0, 1024);
        assert(q);

	int gid = pfq_group_id(q);
	assert(gid != -1);

	struct pfq_stats s;
	assert(pfq_get_group_stats(q, gid, &s) == 0);

	assert(s.recv == 0);
	assert(s.lost == 0);
	assert(s.drop == 0);

	pfq_close(q);
}


void test_my_group_stats_restricted()
{
	pfq_t * q = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_RESTRICTED, 64, 0, 1024);
        assert(q);

	int gid = pfq_group_id(q);
	assert(gid != -1);

	struct pfq_stats s;
	assert(pfq_get_group_stats(q, gid, &s) == 0);

	assert(s.recv == 0);
	assert(s.lost == 0);
	assert(s.drop == 0);

	pfq_close(q);
}


void test_my_group_stats_shared()
{
	pfq_t * q = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_SHARED, 64, 0, 1024);
        assert(q);

	int gid = pfq_group_id(q);
	assert(gid != -1);

	struct pfq_stats s;
	assert(pfq_get_group_stats(q, gid, &s) == 0);

	assert(s.recv == 0);
	assert(s.lost == 0);
	assert(s.drop == 0);

	pfq_close(q);
}


void test_groups_mask()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        assert(q);

	unsigned long groups;
	assert(pfq_groups_mask(q, &groups) == 0);

	assert(groups > 0);

	pfq_close(q);
}

void test_join_restricted()
{
	pfq_t * q = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_RESTRICTED, 64, 0, 1024);
        assert(q);

	pfq_t * y = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED, 64, 0, 1024);

	int gid = pfq_group_id(q);
	assert( pfq_join_group(y, gid, Q_CLASS_DEFAULT, Q_GROUP_RESTRICTED) == gid);

	pfq_close(q);
	pfq_close(y);
}

void test_join_private_()
{
	pfq_t * q = pfq_open(64, 0, 1024);

	pfq_t * y = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED, 64, 0, 1024);

	int gid = pfq_group_id(q);

	assert( pfq_join_group(y, gid, Q_CLASS_DEFAULT, Q_GROUP_PRIVATE) < 0);
	assert( pfq_join_group(y, gid, Q_CLASS_DEFAULT, Q_GROUP_RESTRICTED) < 0);
	assert( pfq_join_group(y, gid, Q_CLASS_DEFAULT, Q_GROUP_SHARED) < 0);
	assert( pfq_join_group(y, gid, Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED) < 0);

	pfq_close(q);
	pfq_close(y);
}

void test_join_restricted_()
{
	{
	pfq_t * q = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_RESTRICTED, 64, 0, 1024);
	pfq_t * y = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED, 64, 0, 1024);

	int gid = pfq_group_id(q);

	assert( pfq_join_group(y, gid, Q_CLASS_DEFAULT, Q_GROUP_PRIVATE) < 0);

	pfq_close(q);
	pfq_close(y);
	}

	{
	pfq_t * q = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_RESTRICTED, 64, 0, 1024);
	pfq_t * y = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED, 64, 0, 1024);

	int gid = pfq_group_id(q);

	assert( pfq_join_group(y, gid, Q_CLASS_DEFAULT, Q_GROUP_RESTRICTED) >= 0);

	pfq_close(q);
	pfq_close(y);
	}

	{
	pfq_t * q = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_RESTRICTED, 64, 0, 1024);
	pfq_t * y = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED, 64, 0, 1024);

	int gid = pfq_group_id(q);

	assert( pfq_join_group(y, gid, Q_CLASS_DEFAULT, Q_GROUP_SHARED) < 0);
	assert( pfq_join_group(y, gid, Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED) < 0);

	pfq_close(q);
	pfq_close(y);
	}
}


void test_join_shared_()
{
	{
	pfq_t * q = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_SHARED, 64, 0, 1024);
	pfq_t * y = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED, 64, 0, 1024);

	int gid = pfq_group_id(q);

	assert( pfq_join_group(y, gid, Q_CLASS_DEFAULT, Q_GROUP_SHARED) >= 0);

	pfq_close(q);
	pfq_close(y);
	}

	{
	pfq_t * q = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_SHARED, 64, 0, 1024);
	pfq_t * y = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED, 64, 0, 1024);

	int gid = pfq_group_id(q);

	assert( pfq_join_group(y, gid, Q_CLASS_DEFAULT, Q_GROUP_PRIVATE) < 0);
	assert( pfq_join_group(y, gid, Q_CLASS_DEFAULT, Q_GROUP_RESTRICTED) < 0);
	assert( pfq_join_group(y, gid, Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED) < 0);

	pfq_close(q);
	pfq_close(y);
	}
}

void test_join_deferred()
{
	pfq_t * q = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED, 64, 0, 1024);
        assert(q);

	assert(pfq_join_group(q, 13, Q_CLASS_DEFAULT, Q_GROUP_SHARED) == 13);
	assert(pfq_join_group(q, 13, Q_CLASS_DEFAULT, Q_GROUP_SHARED) == 13);

	unsigned long mask;

	assert(pfq_groups_mask(q, &mask) == 0);

	assert(mask != 0);
	assert(mask == (1<<13));

	pfq_close(q);
}

void *restricted_thread(void *arg)
{
	pfq_t * q = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED, 64, 0, 1024);
        assert(q);

	long int gid = (long int)arg;
	long int ngid = pfq_join_group(q, gid, Q_CLASS_DEFAULT, Q_GROUP_RESTRICTED);

	assert(ngid == gid);

	pfq_close(q);
	return 0;
}

void test_join_restricted_thread()
{
	pfq_t * q = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_RESTRICTED, 64, 0, 1024);
        assert(q);

	pthread_t t;

	long int gid = pfq_group_id(q);

	assert(pthread_create(&t, NULL, restricted_thread, (void *)gid) == 0);

        pthread_join(t, NULL);

	pfq_close(q);
}

void test_join_restricted_process()
{
	pfq_t * x = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_RESTRICTED, 64, 0, 1024);
	pfq_t * z = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_SHARED, 64, 0, 1024);

	assert(x);
	assert(z);

	int p = fork();
	if (p == 0) {
		pfq_t * y = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED, 64, 0, 1024);

		int gid = pfq_group_id(z);
		assert( pfq_join_group(y, gid, Q_CLASS_DEFAULT, Q_GROUP_SHARED) == gid);
		assert( pfq_join_group(y, pfq_group_id(x), Q_CLASS_DEFAULT, Q_GROUP_SHARED) == -1);

		pfq_close(y);

		_Exit(1);
	}

	wait(NULL);

	pfq_close(x);
	pfq_close(z);
}

void test_join_group()
{
    	pfq_t * q = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED, 64, 0, 1024);
    	assert(q);

	int gid = pfq_join_group(q, 0, Q_CLASS_DEFAULT, Q_GROUP_SHARED);
	assert(gid == 0);

	gid = pfq_join_group(q, Q_ANY_GROUP, Q_CLASS_DEFAULT, Q_GROUP_SHARED);
	assert(gid == 1);

	unsigned long mask;
	assert(pfq_groups_mask(q, &mask) == 0);

	assert(mask == 3);
	pfq_close(q);
}


void test_leave_group()
{
    	pfq_t * q = pfq_open_group(Q_CLASS_DEFAULT, Q_GROUP_UNDEFINED, 64, 0, 1024);
    	assert(q);

	int gid = pfq_join_group(q, 42, Q_CLASS_DEFAULT, Q_GROUP_SHARED);
	assert(gid == 42);

	assert(pfq_leave_group(q, 41) == -1);

	assert(pfq_leave_group(q, 42) == 0);

	assert(pfq_group_id(q) == -1);

	unsigned long mask;
	assert(pfq_groups_mask(q, &mask) == 0);
	assert(mask == 0);

	pfq_close(q);
}

void test_vlan()
{
	pfq_t * q = pfq_open(64, 0, 1024);
        int gid;

	assert(q);

        gid = pfq_group_id(q);

	assert(pfq_vlan_filters_enable(q, gid, 1) == 0);
	assert(pfq_vlan_filters_enable(q, gid, 0) == 0);

	assert(pfq_vlan_set_filter(q, gid, 42) == -1);
	assert(pfq_vlan_reset_filter(q, gid, 42) == -1);

	assert(pfq_vlan_filters_enable(q, gid, 1) == 0);

	assert(pfq_vlan_set_filter(q, gid, 42) == 0);
	assert(pfq_vlan_reset_filter(q, gid, 42) == 0);

	pfq_close(q);
}


void test_group_context()
{
        pfq_t * q = pfq_open(64, 0, 1024);

        int n = 42;

        assert(pfq_set_group_function_context(q, pfq_group_id(q), &n, sizeof(n), 0) == 0);

        int m = 0;

        assert(pfq_get_group_function_context(q, pfq_group_id(q), &m, sizeof(m), 0) == 0);

        assert(n == m);

	pfq_close(q);
}


void test_bind_tx()
{
        pfq_t * q = pfq_open(64, 0, 1024);

        assert(pfq_bind_tx(q, "lo", -1) == 0);
        assert(pfq_bind_tx(q, "unknown", -1) == -1);

        pfq_close(q);
}


void test_start_tx_thread()
{
        pfq_t * q = pfq_open(64, 0, 1024);
        assert(pfq_start_tx_thread(q, 0) == -1);

        assert(pfq_bind_tx(q, "lo", -1) == 0);
        assert(pfq_enable(q) == 0);

        assert(pfq_start_tx_thread(q, 0) == 0);

        pfq_close(q);
}


void test_stop_tx_thread()
{
        pfq_t * q = pfq_open(64, 0, 1024);
        assert(pfq_stop_tx_thread(q) == -1);

        assert(pfq_bind_tx(q, "lo", -1) == 0);
        assert(pfq_enable(q) == 0);

        assert(pfq_start_tx_thread(q, 0) == 0);
        assert(pfq_stop_tx_thread(q) == 0);

        pfq_close(q);
}


void test_wakeup_tx_thread()
{
        pfq_t * q = pfq_open(64, 0, 1024);
        assert(pfq_wakeup_tx_thread(q) == -1);

        assert(pfq_bind_tx(q, "lo", -1) == 0);
        assert(pfq_enable(q) == 0);

        assert(pfq_start_tx_thread(q, 0) == 0);
        assert(pfq_wakeup_tx_thread(q) == 0);

        pfq_close(q);
}


void test_tx_queue_flush()
{
        pfq_t * q = pfq_open(64, 0, 1024);
        assert(pfq_tx_queue_flush(q) == -1);

        assert(pfq_bind_tx(q, "lo", -1) == 0);
        assert(pfq_enable(q) == 0);

        assert(pfq_tx_queue_flush(q) == 0);

        pfq_close(q);
}

#define TEST(test)   fprintf(stdout, "running '%s'...\n", #test); test();

int
main(int argc __attribute__((unused)), char *argv[]__attribute__((unused)))
{
        TEST(test_enable_disable);

	TEST(test_is_enabled);
	TEST(test_ifindex);
	TEST(test_timestamp);
	TEST(test_caplen);
	TEST(test_maxlen);
        TEST(test_offset);
	TEST(test_rx_slots);
	TEST(test_rx_slot_size);
	TEST(test_tx_slots);

	TEST(test_bind_device);
	TEST(test_unbind_device);

	TEST(test_poll);

	TEST(test_read);

	TEST(test_stats);
	TEST(test_group_stats);

        TEST(test_my_group_stats_priv);
	TEST(test_my_group_stats_restricted);
	TEST(test_my_group_stats_shared);

	TEST(test_groups_mask);

	TEST(test_join_private_);
	TEST(test_join_restricted_);
	TEST(test_join_shared_);

	TEST(test_join_deferred);
	TEST(test_join_restricted);
	TEST(test_join_restricted_thread);
	TEST(test_join_restricted_process);

	TEST(test_join_group);
	TEST(test_leave_group);

        TEST(test_vlan);

        TEST(test_group_context);

        TEST(test_bind_tx);
        TEST(test_start_tx_thread);
        TEST(test_stop_tx_thread);
        TEST(test_wakeup_tx_thread);
        TEST(test_tx_queue_flush);

        printf("Tests successfully passed.\n");
    	return 0;
}

