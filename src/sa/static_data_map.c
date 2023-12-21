#define MUTE_LOG_DEBUG 1

#include "static_data_map.h"

// Forward declarations of local functions (alphabetical order)
static int key_cmp(void* key1, void* key2, void* arg);
static size_t key_hash(void* key, void* arg);

void static_data_map_init(static_data_map_t* map) {
    lhash_kv_init(map, NULL, key_hash, key_cmp);
}

void static_data_map_clear(static_data_map_t* map) {
    lhash_kv_clear(map);
}

void static_data_map_insert(static_data_map_t* map, vm_stack_value_t index,
                            vm_stack_value_t resolved_index) {
    lhash_kv_insert(map, (void*)(uintptr_t)index,
                    (void*)(uintptr_t)resolved_index);
}

vm_stack_value_t static_data_map_lookup(static_data_map_t* map,
                                        vm_stack_value_t index) {
    uintptr_t resolved_index;
    lhash_kv_find(map, (void*)(uintptr_t)index,
                  (void**)(uintptr_t)&resolved_index);
    return resolved_index;
}

void static_data_map_iterate(static_data_map_t* map,
                             void (*callback)(vm_stack_value_t,
                                              vm_stack_value_t)) {
    lhash_kv_iter_t iter;
    lhash_kv_iter_init(&iter, map);
    while(!lhash_kv_iter_end(&iter)) {
        uintptr_t current_index;
        uintptr_t current_resolved_index;
        lhash_kv_iter_current(&iter, (void**)(uintptr_t*)&current_index,
                              (void**)(uintptr_t*)&current_resolved_index);
        callback((vm_stack_value_t)current_index,
                 (vm_stack_value_t)current_resolved_index);
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
