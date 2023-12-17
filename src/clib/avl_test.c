#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "avl_kv.h"

#define MAX_RANGE   1024
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
	    sample = 1+(random() % MAX_RANGE);
	    uniq = 1;
	    for (j = 0; uniq && (j < i); j++) {
		if (sample == key[j]) uniq = 0;
	    }
	} while(!uniq);
	key[i] = sample;
	// printf("key[%d] = %ld\n", i, key[i]);
    }
}

// simple swap scramble
void scramble_samples(long* key, size_t size)
{
    size_t n = size;
    while(n--) {
	int i = (random() % size);
	int j = (random() % size);
	long t = key[i];
	key[i] = key[j];
	key[j] = t;
    }
}

int cmp_func(void* key, void* obj)
{
    return ((long)key) - ((long)((avl_kv_node_t*)obj)->key);
}

void print_keys(avl_kv_t *avlk)
{
    avl_kv_iter_t iter;

    printf("Keys: ");    
    avl_kv_iter_init(&iter, avlk);
    while(!avl_kv_iter_end(&iter)) {
	void* key;
	avl_kv_iter_current(&iter, &key, NULL);
	printf("%ld ",(long)key);
	avl_kv_iter_next(&iter);
    }
    printf("\n"); 
}
 
int validate(avl_kv_node_t *n)
{
    if (n == NULL)
	return 1;
    else {
	if (validate((avl_kv_node_t*)n->node.left) &&
	    validate((avl_kv_node_t*)n->node.right))
	    return abs(avl_kv_BF(n)) < 2;
	return 0;
    }
}

void test_samples(long* key, long* data, size_t n)
{
    avl_kv_t store;
    int i;
    size_t expect_size;

    avl_kv_init(&store, NULL, cmp_func);
    
    // insert each sample and check that all samples inserted are found
    // and that size is correct
    expect_size = 0;
    for (i = 0; i < (int)n; i++) {
	if (avl_kv_insert(&store, (void*)key[i], (void*)data[i]) < 0) {
	    printf("unable to insert key[%d]\n", i);
	    exit(1);
	}
	expect_size++;
	if (avl_kv_size(&store) != expect_size) {
	    printf("invalid table size\n");
	    exit(1);
	}
	if (!validate((avl_kv_node_t*)store.avl.root)) {
	    printf("insert key[%d] invalid structure\n", i);
	    exit(1);
	}
    }

    print_keys(&store);

    // now remove the samples
    scramble_samples(key, n);  // remove in random order
    
    for (i = 0; i < (int)n; i++) {
	void* ptr;
	if (!avl_kv_find(&store, (void*)key[i], &ptr)) {
	    printf("remove: key[%d] = %ld was not found\n", i, key[i]);
	    exit(1);
	}
	if (avl_kv_remove(&store, (void*)key[i], &ptr) <= 0) {
	    printf("remove: unable to remove key[%d]\n", i);
	    exit(1);
	}
	expect_size--;
	if (avl_kv_size(&store) != expect_size) {
	    printf("invalid table size\n");
	    exit(1);
	}
	if (!validate((avl_kv_node_t*)store.avl.root)) {
	    printf("remove: key[%d] invalid structure\n", i);
	    exit(1);
	}
    }
    avl_kv_clear(&store);
}


void test()
{
    long key[MAX_SAMPLES];
    generate_unique_samples(key, MAX_SAMPLES);
    test_samples(key, key, MAX_SAMPLES);
}

void test0()
{
    long key[MAX_SAMPLES];
    int i;

    for (i = 0; i < MAX_SAMPLES; i++)
	key[i] = i;
    test_samples(key, key, MAX_SAMPLES);
}

int main(int argc, char** argv)
{
    int i;
    (void) argc;
    (void) argv;

    test0();
    srandom(getpid());
    for (i = 0; i < NUM_TESTS; i++)
	test();
    exit(0);
}
