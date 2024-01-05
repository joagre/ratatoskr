#define MUTE_LOG_DEBUG 1

#include "static_data_map.h"

// Forward declarations of local functions (alphabetical order)
static int key_cmp(void* key, hlink_t* link, void* arg);
static size_t key_hash(void* key, void*);

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

//
// Local functions (alphabetical order)
//

static int key_cmp(void* key, hlink_t* link, void* arg) {
    (void*)arg;
    hlink_kv_t* link_kv	= (hlink_kv_t*)link;
    return (uintptr_t)key == (uintptr_t)link_kv->key ? 0 : 1;
}

static size_t key_hash(void* key, void* arg) {
    (void*)arg;
    return (size_t)((uintptr_t)key);
};
