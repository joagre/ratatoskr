//
// demonstrate the use of the lhash_kv library
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

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

const char* values[] = {
	"1",
	"2",
	"3",
	"4",
	"5",
	"6",
	"7"
};

size_t key_hash(void* key, void* arg)
{
    (void*)arg;
    return (size_t)key;
}

int key_cmp(void* key1, void* key2, void* arg)
{
    (void*)arg;
    return (key1 == key2);
}

int main()
{
    lhash_kv_t h;
    int i;

    lhash_kv_init(&h, NULL, key_hash, key_cmp);

    for (i = 0; i < 7; i++) {
	lhash_kv_insert(&h, (void*) keys[i], (void*) values[i]);
    }

    for (i = 0; i < 7; i++) {
	char* value;
	lhash_kv_find(&h, (void*) keys[i], (void**) &value);
	printf("%s -> %s\n", keys[i], value);
    }

    lhash_kv_clear(&h);

    exit(0);
}
