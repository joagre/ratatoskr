#ifndef __SYMBOL_TABLE_H__
#define __SYMBOL_TABLE_H__

#include <lhash_kv.h>
#include "hm.h"

typedef lhash_kv_t symbol_table_t;

symbol_table_t* symbol_table_new(void);
void symbol_table_init(symbol_table_t* symbol_table);
void symbol_table_free(symbol_table_t* symbol_table);
void symbol_table_clear(symbol_table_t* symbol_table);
void symbol_table_insert(symbol_table_t* table, char* name, hm_type_t* type);
hm_type_t* symbol_table_lookup(symbol_table_t* symbol_table, char* name);
void symbol_table_merge(symbol_table_t* symbol_table, symbol_table_t* other);
void symbol_table_print(symbol_table_t* symbol_table);
void symbol_table_unit_test(void);

#endif
