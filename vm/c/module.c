#define MUTE_LOG_DEBUG 1

#include <stdio.h>
#include <stdlib.h>
#include "log.h"
#include "module.h"

// Forward declarations of local functions (alphabetical order)
static int key_cmp(void* key1, void* key2, void* arg);
static size_t key_hash(void* key, void* arg);

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

void module_insert(module_t* module, vm_label_t label, vm_address_t address) {
    lhash_kv_insert(&module->jump_table, (void*)(uintptr_t)label,
                    (void*)(uintptr_t)address);
}

vm_address_t module_lookup_address(module_t* module, vm_label_t label) {
    //uintptr_t address;
    //fprintf(stderr, "PRINT:\n");
    //module_print_jump_table(module);
    // lhash_kv_find(&module->jump_table, &label, (void**)&address);
    //return address;

    // FIXME: This is a workaround
    lhash_kv_iter_t iter;
    lhash_kv_iter_init(&iter, &module->jump_table);
    while(!lhash_kv_iter_end(&iter)) {
        uintptr_t current_label;
        uintptr_t current_address;
        lhash_kv_iter_current(&iter, (void**)(uintptr_t*)&current_label,
                              (void**)(uintptr_t*)&current_address);
        if (current_label == label) {
            return current_address;
        }

        lhash_kv_iter_next(&iter);
    }
    LOG_ABORT("Label address mismatch");
    return 0;
}

vm_label_t module_lookup_label(module_t* module, vm_address_t address) {
    lhash_kv_iter_t iter;
    lhash_kv_iter_init(&iter, &module->jump_table);
    while(!lhash_kv_iter_end(&iter)) {
        uintptr_t current_label;
        uintptr_t current_address;
        lhash_kv_iter_current(&iter, (void**)(uintptr_t*)&current_label,
                              (void**)(uintptr_t*) &current_address);
        if (address == current_address) {
            return current_label;
        }
        lhash_kv_iter_next(&iter);
    }
    LOG_ABORT("Label address mismatch");
    return 0;
}
void module_print_jump_table(module_t* module) {
    lhash_kv_iter_t iter;
    lhash_kv_iter_init(&iter, &module->jump_table);
    while(!lhash_kv_iter_end(&iter)) {
        uintptr_t current_label;
        uintptr_t current_address;
        lhash_kv_iter_current(&iter, (void**)(uintptr_t*)&current_label,
                              (void**)(uintptr_t*)&current_address);
        fprintf(stderr, "Label %d at address %d\n", (vm_label_t) current_label,
                (vm_address_t)current_address);
        lhash_kv_iter_next(&iter);
    }
}

//
// Local functions (alphabetical order)
//

static int key_cmp(void* key1, void* key2, void* arg) {
    (void*)arg;
    return (uintptr_t)key1 == (uintptr_t)key2;
};

static size_t key_hash(void* key, void* arg) {
    (void*)arg;
    return (size_t)((uintptr_t)key);
};

//
// Unit test
//

void module_unit_test(void) {
    module_t* module = module_new(4711);
    vm_label_t label = 42;

    // insert
    vm_address_t address = 8;
    module_insert(module, label, address);

    // lookup
    vm_address_t address2 = module_lookup_address(module, label);
    LOG_ASSERT(address == address2, "Address mismatch");
    vm_label_t label2 = module_lookup_label(module, address);
    LOG_ASSERT(label == label2, "Label mismatch");
    module_print_jump_table(module);

    LOG_INFO("Unit test passed");
}
