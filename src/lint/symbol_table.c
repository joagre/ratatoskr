//#define MUTE_LOG_DEBUG 1

#include <log.h>
#include "symbol_table.h"

// Forward declarations of local functions (alphabetical order)
static int key_cmp(void* key1, void* key2, void*);
static size_t key_hash(void* key, void*);

symbol_table_t* symbol_table_new(void) {
    symbol_table_t* table = malloc(sizeof(symbol_table_t));
    symbol_table_init(table);
    return table;
}

void symbol_table_init(symbol_table_t* table) {
    lhash_kv_init(table, NULL, key_hash, key_cmp);
}

void symbol_table_free(symbol_table_t* table) {
    symbol_table_clear(table);
    free(table);
}

void symbol_table_clear(symbol_table_t* table) {
    lhash_kv_clear(table);
}

void symbol_table_insert(symbol_table_t* table, char* name,
			 type_variable_t variable) {
    lhash_kv_insert(table, (void*)name, (void*)(uintptr_t)variable);
}

type_variable_t symbol_table_lookup(symbol_table_t* table, char* name) {
    uintptr_t variable = 0;
    lhash_kv_find(table, (void*)name, (void**)&variable);
    return variable;
}

void symbol_table_merge(symbol_table_t* table, symbol_table_t* other) {
    lhash_kv_iter_t iter;
    lhash_kv_iter_init(&iter, other);
    while (!lhash_kv_iter_end(&iter)) {
	char* name;
	uintptr_t variable;
	lhash_kv_iter_current(&iter, (void**)&name, (void**)(uintptr_t*)&variable);
	symbol_table_insert(table, name, variable);
	lhash_kv_iter_next(&iter);
    }
}

void symbol_table_print(symbol_table_t* table) {
    lhash_kv_iter_t iter;
    lhash_kv_iter_init(&iter, table);
    while (!lhash_kv_iter_end(&iter)) {
	char* name;
	uintptr_t variable;
	lhash_kv_iter_current(&iter, (void**)&name, (void**)(uintptr_t*)&variable);
	fprintf(stderr, "%s: %ld\n", name, variable);
	lhash_kv_iter_next(&iter);
    }
}

//
// Local functions (alphabetical order)
//

static int key_cmp(void* key1, void* key2, void* arg) {
    return (key1 == key2);
};

static size_t key_hash(void* key, void* arg) {
    size_t hash = 0;
    int c;
    while ((c = *(char*)key++)) {
        hash = c + (hash << 6) + (hash << 16) - hash;
    }
    return hash;
    //return (size_t)key;
};

//
// Unit test
//

void symbol_table_unit_test(void) {
    symbol_table_t table;
    symbol_table_init(&table);
    symbol_table_insert(&table, "foo", 42);
    symbol_table_insert(&table, "bar", 4711);
    symbol_table_print(&table);
    type_variable_t variable = symbol_table_lookup(&table, "foo");
    LOG_ASSERT(variable == 42, "Wrong variable");
    LOG_INFO("Unit test passed");
}
