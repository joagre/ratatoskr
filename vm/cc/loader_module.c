#include "vm.h"
#include "log.h"
#include "loader_module.h"

size_t key_hash(void* key, void*) {
    return (size_t)key;
};

int key_cmp(void* key1, void* key2, void*) {
    return (key1 == key2);
};

module_t* loader_module_new(vm_address_t start_address) {
    module_t* module = malloc(sizeof(module_t));
    module->start_address = start_address;
    lhash_kv_init(&module->jump_table, NULL, key_hash, key_cmp);
    return module;
}

void loader_module_free(module_t *module) {
    lhash_kv_clear(&module->jump_table);
}

void loader_module_insert_label(module_t* module, vm_label_t label,
                                vm_address_t address) {
    lhash_kv_insert(&module->jump_table, (void *)&label, (void*)&address);
}

vm_address_t loader_module_lookup_address(module_t* module, vm_label_t label) {
    vm_address_t* address;
    lhash_kv_find(&module->jump_table, (void*)&label, (void**)&address);
    return *address;
}

vm_label_t loader_module_lookup_label(module_t* module, vm_address_t address) {
    lhash_iter_t iter;
    lhash_kv_iter_init(&iter, &module->jump_table);
    while(!lhash_kv_iter_end(&iter)) {
        vm_label_t* current_label;
        vm_address_t* current_address;
        lhash_kv_iter_current(&iter, (void*)&current_label,
                              (void*)&current_address);
        if (address == *current_address) {
            return *current_label;
        }
    }
    SATIE_ABORT("Label address mismatch");
    return 0;
}
