//
// demonstrate the use of the lhash_kv library
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define LHASH_KV_KEY_TYPE   const char*
#define LHASH_KV_VALUE_TYPE int

#include "lhash_kv.h"

const char* keys[] = {
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven"
};

const int values[] = {
    1,
    2,
    3,
    4,
    5,
    6,
    7
};

size_t key_hash(void* key, void* arg)
{
    (void)arg;
    return (size_t)key;
}

int key_cmp(void* key, hlink_t* obj, void* arg)
{
    (void)arg;    
    return !(key == ((hlink_kv_t*)obj)->key);
}

int main()
{
    lhash_kv_t h;
    int i;

    lhash_kv_init(&h, NULL, key_hash, key_cmp);
    
    for (i = 0; i < 7; i++) {
	lhash_kv_insert(&h, keys[i], values[i]);
    }

    for (i = 0; i < 7; i++) {
	int value;
	lhash_kv_find(&h, keys[i], &value);
	printf("%s -> %d\n", keys[i], value);
    }

    lhash_kv_clear(&h);

    exit(0);
}
