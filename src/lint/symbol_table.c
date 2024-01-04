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

hm_type_variable_t symbole_table_next_variable(symbol_table_t* table) {
    static uint32_t counter = 0;
    return counter++;
}

void symbol_table_insert(symbol_table_t* table, char* name, hm_type_t* type) {
    lhash_kv_insert(table, (void*)name, (void*)type);
}

hm_type_t* symbol_table_lookup(symbol_table_t* table, char* name) {
    hm_type_t* type = NULL;
    lhash_kv_find(table, (void*)name, (void**)&type);
    return type;
}

void symbol_table_merge(symbol_table_t* table, symbol_table_t* other) {
    lhash_kv_iter_t iter;
    lhash_kv_iter_init(&iter, other);
    while (!lhash_kv_iter_end(&iter)) {
	char* name;
	hm_type_t* type;
	lhash_kv_iter_current(&iter, (void**)&name, (void**)&type);
	symbol_table_insert(table, name, type);
	lhash_kv_iter_next(&iter);
    }
}

void symbol_table_print(symbol_table_t* table) {
    lhash_kv_iter_t iter;
    lhash_kv_iter_init(&iter, table);
    while (!lhash_kv_iter_end(&iter)) {
	char* name;
	hm_type_t* type;
	lhash_kv_iter_current(&iter, (void**)&name, (void**)&type);
	fprintf(stderr, "%s: ", name);
	switch (type->tag) {
	    case HM_TYPE_TAG_BASIC_TYPE:
		switch (type->basic_type) {
		    case HM_TYPE_INTEGRAL:
			fprintf(stderr, "integral\n");
			break;
		    case HM_TYPE_BOOL:
			fprintf(stderr, "bool\n");
			break;
		}
		break;
	    case HM_TYPE_TAG_VARIABLE:
		fprintf(stderr, "t%d\n", type->variable);
		break;
	}
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
};

//
// Unit test
//

void symbol_table_unit_test(void) {
    symbol_table_t table;
    symbol_table_init(&table);
    hm_type_t* type1 = malloc(sizeof(hm_type_t));
    type1->tag = HM_TYPE_TAG_BASIC_TYPE;
    type1->basic_type = HM_TYPE_INTEGRAL;
    symbol_table_insert(&table, "int", type1);
    hm_type_t* type2 = malloc(sizeof(hm_type_t));
    type2->tag = HM_TYPE_TAG_BASIC_TYPE;
    type2->basic_type = HM_TYPE_BOOL;
    symbol_table_insert(&table, "bool", type2);
    hm_type_t* type3 = malloc(sizeof(hm_type_t));
    type3->tag = HM_TYPE_TAG_VARIABLE;
    type3->variable = 4711;
    symbol_table_insert(&table, "bar", type3);
    hm_type_t* type = symbol_table_lookup(&table, "int");
    LOG_ASSERT(type->tag == HM_TYPE_TAG_BASIC_TYPE, "Wrong type");
    LOG_INFO("Unit test passed");
}
