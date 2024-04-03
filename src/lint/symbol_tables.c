//#define MUTE_LOG_DEBUG 1

#include <log.h>
#include "symbol_tables.h"

void symbol_tables_init(symbol_tables_t* tables) {
    dynarray_init(tables, NULL, 0, sizeof(symbol_tables_entry_t));
}

void symbol_tables_clear(symbol_tables_t* tables) {
    dynarray_clear(tables);
}

type_t* symbol_tables_lookup(symbol_tables_t* tables, char* name) {
    uint16_t n = dynarray_size(tables);
    LOG_ASSERT(n > 0, "There is no symbol table available");
    uint16_t i = n - 1;
    do {
	symbol_tables_entry_t* entry = dynarray_element(tables, i);
	type_t* type = symbol_table_lookup(entry->table, name);
	if (type != NULL) {
	    return type;
	}
    } while (i-- > 0);
    return NULL;
}

void symbol_tables_insert_table(symbol_tables_t* tables, symbol_table_t* table,
				uint32_t id) {
    symbol_tables_entry_t entry;
    entry.id = id;
    entry.table = table;
    dynarray_append(tables, &entry);
}

void symbol_tables_insert(symbol_tables_t* tables, char* name, type_t* type) {
    uint16_t n = dynarray_size(tables);
    LOG_ASSERT(n > 0, "There is no symbol table available");
    symbol_tables_entry_t* entry = dynarray_element(tables, n - 1);
    symbol_table_insert(entry->table, name, type);
}

void symbol_tables_delete_by_id(symbol_tables_t* tables, uint32_t id) {
    uint16_t n = dynarray_size(tables);
    while (n-- > 0) {
	symbol_tables_entry_t* entry = dynarray_element(tables, n);
	dynarray_delete(tables, n);
	if (entry->id == id) {
	    break;
	}
    }
}

void symbol_tables_hoist(symbol_tables_t* tables, char* name) {
    uint16_t n = dynarray_size(tables);
    LOG_ASSERT(n > 1, "There is no symbol table available for hoisting");
    uint16_t i = n - 1;
    do {
	symbol_tables_entry_t* entry = dynarray_element(tables, i);
	type_t* type = symbol_table_lookup(entry->table, name);
	if (type != NULL) {
	    type_t* type_copy = malloc(sizeof(type_t));
	    memcpy(type_copy, type, sizeof(type_t));
	    //printf("Hoisting %s\n", name);
	    symbol_table_delete(entry->table, name);
	    LOG_ASSERT(i > 0, "Cannot hoist symbol table to the top");
	    symbol_tables_entry_t* entry_above = dynarray_element(tables, i - 1);
	    symbol_table_insert(entry_above->table, name, type_copy);
	    return;
	}
    } while (i-- > 0);
    LOG_ABORT("Symbol %s not found", name);
}

void symbol_tables_print(symbol_tables_t* tables) {
    uint16_t n = dynarray_size(tables);
    printf("====\n");
    for (uint16_t i = 0; i < n; i++) {
	symbol_tables_entry_t* entry = dynarray_element(tables, i);
	printf("== %d\n", entry->id);
	symbol_table_print(entry->table);
    }
}
