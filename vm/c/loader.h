#ifndef __LOADER_H__
#define __LOADER_H__

#include <stdint.h>
#include <stdio.h>
#include "clib/lhash_kv.h"
#include "module.h"
#include "satie.h"

#define APPEND_VALUE(loader, T, value) ({    \
    uint8_t bytes[sizeof(T)]; \
    memcpy(bytes, &(value), sizeof(T)); \
    append_bytes(loader, sizeof(T), bytes); \
})

typedef lhash_kv_t module_map_t;

typedef struct {
    uint8_t* byte_code;
    uint32_t byte_code_size;
    uint32_t max_byte_code_size;
    char* load_path;
    lhash_kv_t modules;
} loader_t;

void loader_init(loader_t* loader, char* load_path);
void loader_free(loader_t* loader);
bool loader_is_module_loaded(loader_t* loader, char *module_name);
vm_address_t loader_lookup_address(loader_t* loader, char *module_name,
                                   vm_label_t label);
void loader_load_module(loader_t *loader, char* module_name,
                        satie_error_t* error);
void pretty_print(loader_t* loader);
void pretty_print_module(loader_t* loader, char* module_name);
void loader_unit_test(void);

#endif
