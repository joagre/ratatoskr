#ifndef __MODULE_H__
#define __MODULE_H__

#include "clib/lhash_kv.h"
#include "vm.h"

typedef struct {
    vm_address_t start_address;
    vm_address_t stop_address;
    lhash_kv_t jump_table;
} module_t;

module_t* module_new(vm_address_t start_address);
void module_free(module_t *);
void module_insert_label(module_t* module, vm_label_t label,
                         vm_address_t address);
vm_address_t module_lookup_address(module_t* module, vm_label_t label);
vm_label_t module_lookup_label(module_t* module, vm_address_t address);

#endif
