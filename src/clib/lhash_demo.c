//
// demonstrate the use of the lhash_kv library
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "lhash.h"

typedef struct {
    hlink_t link;
    const char* key;
    const char* value;
} kvlink_t;
   
static kvlink_t kv[] = {
    { .key="one",   .value="1" },
    { .key="two",   .value="2" },
    { .key="three", .value="3" },
    { .key="four",  .value="4" },
    { .key="five",  .value="5" },
    { .key="six",   .value="6" },
    { .key="seven", .value="7" },
};

size_t key_hash(void* key, void* arg)
{
    (void)arg;
    return (size_t)key;
}

// a bit different than lhash_hv, cmp = 0 means equal
int key_cmp(void* key, hlink_t* obj, void* arg)
{
    (void)arg;
    return ((size_t) key - (size_t)((kvlink_t*)obj)->key);
}

int main()
{
    lhash_t h;
    int i;

    lhash_init(&h, NULL, key_hash, key_cmp);
    
    for (i = 0; i < 7; i++) {
	lhash_insert(&h, (void*) kv[i].key,  (void*) &kv[i]);
    }

    for (i = 0; i < 7; i++) {
	kvlink_t* ptr;
	lhash_find(&h, (void*) kv[i].key, (void**) &ptr);
	printf("%s -> %s\n", ptr->key, ptr->value);
    }

    lhash_clear(&h);

    exit(0);
}
