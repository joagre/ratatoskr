#include <stdio.h>
#include <stdlib.h>
#include "log.h"
#include "module.h"

// Forward declarations of local functions (alphabetical order)
static int key_cmp(void* key1, void* key2, void*);
static size_t key_hash(void* key, void*);

module_t* module_new(vm_address_t start_address) {
    module_t* module = malloc(sizeof(module_t));
    module->start_address = start_address;
    lhash_kv_init(&module->jump_table, NULL, key_hash, key_cmp);
    return module;
}

void module_free(module_t *module) {
    lhash_kv_clear(&module->jump_table);
    free(module);
}

void module_insert_label(module_t* module, vm_label_t label,
                         vm_address_t address) {
    lhash_kv_insert(&module->jump_table, (void*)(uintptr_t)label,
                    (void*)(uintptr_t)address);
}

vm_address_t module_lookup_address(module_t* module, vm_label_t label) {
    uintptr_t address;
    lhash_kv_find(&module->jump_table, &label, (void **)&address);
    return address;
}

vm_label_t module_lookup_label(module_t* module, vm_address_t address) {
    lhash_kv_iter_t iter;
    lhash_kv_iter_init(&iter, &module->jump_table);
    while(!lhash_kv_iter_end(&iter)) {
        vm_label_t current_label;
        vm_address_t current_address;
        lhash_kv_iter_current(&iter, (void**)(uintptr_t*)&current_label,
                              (void**)(uintptr_t*)&current_address);
        if (address == current_address) {
            return current_label;
        }
        lhash_kv_iter_next(&iter);
    }
    SATIE_ABORT("Label address mismatch");
    return 0;
}
void module_print_jump_table(module_t* module) {
    lhash_kv_iter_t iter;
    lhash_kv_iter_init(&iter, &module->jump_table);
    while(!lhash_kv_iter_end(&iter)) {
        vm_label_t current_label;
        vm_address_t current_address;
        lhash_kv_iter_current(&iter, (void**)(uintptr_t*)&current_label,
                              (void**)(uintptr_t*)&current_address);
        fprintf(stderr, "Label %d at address %d\n", current_label,
                current_address);
        lhash_kv_iter_next(&iter);
    }
}

//
// Local functions (alphabetical order)
//

static int key_cmp(void* key1, void* key2, void*) {
    return (uintptr_t)key1 == (uintptr_t)key2;
};

static size_t key_hash(void* key, void*) {
    return (size_t)((uintptr_t)key);
};

//
// Unit test
//

void module_unit_test(void) {
    module_t* module = module_new(4711);
    vm_label_t label = 42;
    vm_address_t address = 8;
    module_insert_label(module, label, address);
    vm_address_t address2 = module_lookup_address(module, label);
    SATIE_ASSERT(address == address2, "Address mismatch");
    vm_label_t label2 = module_lookup_label(module, address);
    SATIE_ASSERT(label == label2, "Label mismatch");
    module_print_jump_table(module);
    SATIE_LOG(LOG_LEVEL_INFO, "module_unit_test passed");
}