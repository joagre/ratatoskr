#ifndef __SYMBOL_TABLES_H__
#define __SYMBOL_TABLES_H__

#include <dynarr.h>
#include "symbol_table.h"

typedef dynarray_t symbol_tables_t;

typedef struct {
    uint32_t id;
    symbol_table_t* table;
} symbol_tables_entry_t;

void symbol_tables_init(symbol_tables_t* tables);
void symbol_tables_clear(symbol_tables_t* tables);
type_t* symbol_tables_lookup(symbol_tables_t* tables, char* name);
void symbol_tables_insert_table(symbol_tables_t* tables, symbol_table_t* table,
				uint32_t id);
void symbol_tables_insert(symbol_tables_t* tables, char* name, type_t* type);
uint16_t symbol_tables_delete_by_id(symbol_tables_t* tables, uint32_t id);
void symbol_tables_print(symbol_tables_t* tables);

#endif
