#ifndef SA_MODULE_H
#define SA_MODULE_H

#include <lhash_kv.h>
#include "vm.h"

typedef struct {
    vm_address_t start_address;
    vm_address_t stop_address;
    lhash_kv_t jump_table;
} module_t;

void module_init(module_t* module, vm_address_t start_address);
void module_clear(module_t *module);
module_t* module_new(vm_address_t start_address);
void module_free(module_t *module);
uint32_t module_jump_table_size(module_t* module);
void module_insert(module_t* module, vm_label_t label, vm_address_t address);
vm_address_t module_lookup_address(module_t* module, vm_label_t label);
vm_label_t module_lookup_label(module_t* module, vm_address_t address);
void module_iterate(module_t* module,
                    void (*callback)(vm_label_t, vm_address_t));
void module_print_jump_table(module_t* module);
void module_unit_test(void);

#endif
