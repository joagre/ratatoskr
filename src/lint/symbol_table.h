#ifndef LINT_SYMBOL_TABLE_H
#define LINT_SYMBOL_TABLE_H

#include <lhash_kv.h>
#include "type.h"

typedef lhash_kv_t symbol_table_t;

symbol_table_t* symbol_table_new(void);
void symbol_table_init(symbol_table_t* table);
void symbol_table_free(symbol_table_t* table);
void symbol_table_clear(symbol_table_t* table);
void symbol_table_insert(symbol_table_t* table, char* name, type_t* type);
void symbol_table_delete(symbol_table_t* table, char* name);
type_t* symbol_table_lookup(symbol_table_t* table, char* name);
void symbol_table_merge(symbol_table_t* table, symbol_table_t* other);
void symbol_table_print(symbol_table_t* table);
void symbol_table_unit_test(void);

#endif
