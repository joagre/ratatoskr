#ifndef __MODULE_H__
#define __MODULE_H__

#include "clib/lhash_kv.h"
#include "vm.h"

typedef struct {
    address_type_t start_address;
    address_type_t stop_address;
    lhash_kv_t jump_table;
} module_t;

module_t* module_new(address_type_t start_address);
void module_free(module_t* module);

#endif
