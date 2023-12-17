#include <assert.h>
#include "lhash_kv.h"

typedef uint32_t vm_label_t;
typedef uint32_t vm_address_t;

typedef struct {
    vm_address_t start_address;
    vm_address_t stop_address;
    lhash_kv_t jump_table;
} module_t;

static size_t key_hash(void* key, void*) {
    return (size_t)((uintptr_t)key);
};

static int key_cmp(void* key1, void* key2, void*) {
    return (uintptr_t)key1 == (uintptr_t)key2;
};

module_t* module_new(vm_address_t start_address) {
    module_t* module = malloc(sizeof(module_t));
    module->start_address = start_address;
    lhash_kv_init(&module->jump_table, NULL, key_hash, key_cmp);
    return module;
}

void module_insert_label(module_t* module, vm_label_t label,
                         vm_address_t address) {
    lhash_kv_insert(&module->jump_table, (void*)(uintptr_t)label, (void*)(uintptr_t)address);
}

vm_address_t module_lookup_address(module_t* module, vm_label_t label) {
    uintptr_t address;
    lhash_kv_find(&module->jump_table, &label, (void **)&address);
    return address;
}

int main(void) {
    module_t* module = module_new(4711);
    vm_label_t label = 42;
    vm_address_t address = 8;
    module_insert_label(module, label, address);
    vm_address_t address2 = module_lookup_address(module, label);
    assert(address == address2);
}
