#include <assert.h>
#include <stdint.h>

typedef uint32_t vm_label_t;
typedef uint32_t vm_address_t;

#define LHASH_KV_KEY_TYPE   vm_label_t
#define LHASH_KV_VALUE_TYPE vm_address_t

#include "lhash_kv.h"

typedef struct {
    vm_address_t start_address;
    vm_address_t stop_address;
    lhash_kv_t jump_table;
} module_t;

static size_t key_hash(void* key, void*) {
    return (size_t)((uintptr_t)key);
};

static int key_cmp(void* key, hlink_t* obj, void*) {
    return (uintptr_t)key - ((hlink_kv_t*)obj)->key;
};

module_t* module_new(vm_address_t start_address) {
    module_t* module = malloc(sizeof(module_t));
    module->start_address = start_address;
    lhash_kv_init(&module->jump_table, NULL, key_hash, key_cmp);
    return module;
}

void module_insert_label(module_t* module, vm_label_t label,
                         vm_address_t address) {
    lhash_kv_insert(&module->jump_table, label, address);
}

vm_address_t module_lookup_address(module_t* module, vm_label_t label) {
    vm_address_t address;
    lhash_kv_find(&module->jump_table, label, &address);
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
