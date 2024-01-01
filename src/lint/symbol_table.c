#define MUTE_LOG_DEBUG 1

#include <log.h>
#include "symbol_table.h"

// Forward declarations of local functions (alphabetical order)
static int key_cmp(void* key1, void* key2, void*);
static size_t key_hash(void* key, void*);

symbol_table_t* symbol_table_new(void) {
    symbol_table_t* symbol_table = malloc(sizeof(symbol_table_t));
    symbol_table_init(symbol_table);
    return symbol_table;
}

void symbol_table_init(symbol_table_t* symbol_table) {
    lhash_kv_init(symbol_table, NULL, key_hash, key_cmp);
}

void symbol_table_free(symbol_table_t* symbol_table) {
    symbol_table_clear(symbol_table);
    free(symbol_table);
}

void symbol_table_clear(symbol_table_t* symbol_table) {
    lhash_kv_clear(symbol_table);
}

void symbol_table_insert(symbol_table_t* symbol_table, char* name,
			 type_variable_t variable) {
    fprintf(stderr, "inserting '%s' (%d)\n", name, variable);
    lhash_kv_insert(symbol_table, (void*)name,
		    (void*)(uintptr_t)variable);
    fprintf(stderr, "inserted '%s'\n", name);
}

type_variable_t symbol_table_lookup(symbol_table_t* symbol_table, char* name) {
    type_variable_t variable = 0;
    lhash_kv_find(symbol_table, (void*)name,
		  (void**)(uintptr_t)&variable);
    return variable;
}

void symbol_table_merge(symbol_table_t* symbol_table, symbol_table_t* other) {
    lhash_kv_iter_t iter;
    lhash_kv_iter_init(&iter, other);
    while (!lhash_kv_iter_end(&iter)) {
	char* name;
	type_variable_t variable;
	lhash_kv_iter_current(&iter, (void**)&name, (void**)&variable);
	symbol_table_insert(symbol_table, name, variable);
	lhash_kv_iter_next(&iter);
    }
}

void symbol_table_print(symbol_table_t* symbol_table) {
    lhash_kv_iter_t iter;
    lhash_kv_iter_init(&iter, symbol_table);
    while (!lhash_kv_iter_end(&iter)) {
	char* name;
	type_variable_t variable;
	lhash_kv_iter_current(&iter, (void**)&name, (void**)&variable);
	fprintf(stderr, "%s: %d\n", name, variable);
	lhash_kv_iter_next(&iter);
    }
}

//
// Local functions (alphabetical order)
//

static int key_cmp(void* key1, void* key2, void* arg) {
    (void*)arg;
    //fprintf(stderr, "COMPARE: %s == %s\n ==> %d", (char*)key1, (char*)key2, strcmp(key1, key2));
    //return (strcmp(key1, key2) == 0);
    return (key1 == key2);
};

static size_t key_hash(void* key, void* arg) {
    //fprintf(stderr, "HASH: %s\n", (char*)key);
    (void*)arg;
    return (size_t)key;
};

//
// Unit test
//

void symbol_table_unit_test(void) {
    symbol_table_t table;
    symbol_table_init(&table);
    symbol_table_insert(&table, "foo", 42);
    symbol_table_insert(&table, "bar", 4711);
    type_variable_t variable = symbol_table_lookup(&table, "foo");
    LOG_ASSERT(variable == 42, "Wrong variable");
    LOG_INFO("Unit test passed");
}
