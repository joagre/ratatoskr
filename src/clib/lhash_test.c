// random test lhash implementation

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "lhash_kv.h"

#define MAX_SAMPLES 100
#define NUM_TESTS 10

void generate_unique_samples(long* key, size_t size)
{
    int i;
    for (i = 0; i < (int)size; i++) {
	int uniq;
	long sample;
	do {
	    int j;
	    sample = random();
	    uniq = 1;
	    for (j = 0; uniq && (j < i); j++) {
		if (sample == key[j]) uniq = 0;
	    }
	} while(!uniq);
	key[i] = sample;
	// printf("key[%d] = %ld\n", i, key[i]);
    }
}

lhash_value_t hash_func(void *key, void* arg)
{
    (void) arg;
    return (lhash_value_t)((long)key);
}

int cmp_func(void *key, hlink_t* obj, void* arg)
{
    (void) arg;
    return ((long)key) - ((long)((hlink_kv_t*)obj)->key);
}

void test()
{
    long key[MAX_SAMPLES];
    lhash_t store;
    int i;
    size_t expect_size;
    
    generate_unique_samples(key, MAX_SAMPLES);

    lhash_kv_init(&store, NULL, hash_func, cmp_func);
    
    // insert each sample and check that all samples inserted are found
    // and that size is correct
    expect_size = 0;
    for (i = 0; i < MAX_SAMPLES; i++) {
	lhash_iter_t iter;
	
	// printf("insert key[%d] = %ld, data = %ld\n", i, key[i], -key[i]);

	// check if key[i] is present
	lhash_kv_iter_init(&iter, &store);
	while(!lhash_kv_iter_end(&iter)) {
	    hlink_kv_t kv;
	    void* k;
	    void* d;
	    lhash_kv_iter_current(&iter, &k, &d);
	    kv.key = (void*)key[i];
	    if ((*cmp_func)(k, (hlink_t*)&kv, &store) == 0) {
		printf("key[%d] = %ld already present\n", i, key[i]);
		exit(1);
	    }
	    lhash_kv_iter_next(&iter);
	}

	if (lhash_kv_insert(&store, (void*)key[i], (void*)-key[i]) < 0) {
	    printf("unable to insert key[%d]\n", i);
	    exit(1);
	}
	expect_size++;
	if (lhash_kv_size(&store) != expect_size) {
	    printf("invalid table size\n");
	    exit(1);
	}
    }

    // now remove that samples
    for (i = 0; i < MAX_SAMPLES; i++) {
	void* ptr;
	if (!lhash_kv_find(&store, (void*)key[i], &ptr)) {
	    printf("key[%d] = %ld was not found\n", i, key[i]);
	    exit(1);
	}
	if (lhash_kv_remove(&store, (void*)key[i], &ptr) <= 0) {
	    printf("unable to remove key[%d]\n", i);
	    exit(1);
	}
	expect_size--;
	if (lhash_kv_size(&store) != expect_size) {
	    printf("invalid table size\n");
	    exit(1);
	}	
    }
    
    lhash_kv_clear(&store);
}

int main(int argc, char** argv)
{
    int i;
    (void) argc;
    (void) argv;

    srandom(getpid());
    for (i = 0; i < NUM_TESTS; i++)
	test();
    exit(0);
}
