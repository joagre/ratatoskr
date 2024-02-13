//#define MUTE_LOG_DEBUG 1

#include <log.h>
#include "symbol_tables.h"

void symbol_tables_init(symbol_tables_t* tables) {
    //fprintf(stderr, "symbol_tables_init\n");
    dynarray_init(tables, NULL, 0, sizeof(symbol_tables_entry_t));
}

void symbol_tables_clear(symbol_tables_t* tables) {
    //fprintf(stderr, "symbol_tables_clear\n");
    dynarray_clear(tables);
}

type_t* symbol_tables_lookup(symbol_tables_t* tables, char* name) {
    //fprintf(stderr, "symbol_tables_lookup\n");
    size_t n = dynarray_size(tables);
    LOG_ASSERT(n > 0, "There is no symbol table available");
    size_t i = n - 1;
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
    //fprintf(stderr, "symbol_tables_insert_table\n");
    symbol_tables_entry_t entry;
    entry.id = id;
    entry.table = table;
    dynarray_append(tables, &entry);
}

void symbol_tables_insert(symbol_tables_t* tables, char* name, type_t* type) {
    //fprintf(stderr, "symbol_tables_insert\n");
    size_t n = dynarray_size(tables);
    LOG_ASSERT(n > 0, "There is no symbol table available");
    symbol_tables_entry_t* entry = dynarray_element(tables, n - 1);
    symbol_table_insert(entry->table, name, type);
}

uint16_t symbol_tables_delete_by_id(symbol_tables_t* tables, uint32_t id) {
    //fprintf(stderr, "symbol_tables_delete_by_id\n");
    size_t n = dynarray_size(tables);
    uint16_t deleted_entries = 0;
    for (size_t i = 0; i < n; i++) {
	symbol_tables_entry_t* entry = dynarray_element(tables, i);
	if (entry->id == id) {
	    //symbol_table_free(entry->table);
	    dynarray_delete(tables, i);
	    deleted_entries++;
	}
    }
    return deleted_entries;
}

void symbol_tables_print(symbol_tables_t* tables) {
    //fprintf(stderr, "symbol_tables_print\n");
    size_t n = dynarray_size(tables);
    fprintf(stderr, "====\n");
    for (size_t i = 0; i < n; i++) {
	symbol_tables_entry_t* entry = dynarray_element(tables, i);
	fprintf(stderr, "== %d\n", entry->id);
	symbol_table_print(entry->table);
    }
}
