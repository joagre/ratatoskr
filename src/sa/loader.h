#ifndef __LOADER_H__
#define __LOADER_H__

#include <stdint.h>
#include <stdio.h>
#include <lhash_kv.h>
#include <satie_error.h>
#include "module.h"
#include "static_data.h"
#include "static_data_map.h"

#define APPEND_VALUE(loader, T, value) ({    \
    uint8_t bytes[sizeof(T)]; \
    memcpy(bytes, &(value), sizeof(T)); \
    append_bytes(loader, sizeof(T), bytes); \
})

typedef lhash_kv_t module_map_t;

typedef struct {
    uint8_t* bytecode;
    uint32_t bytecode_size;
    uint32_t max_bytecode_size;
    char* load_path;
    lhash_kv_t modules;
    static_data_t static_data;
    static_data_map_t static_data_map;
} loader_t;

void loader_init(loader_t* loader, char* load_path);
void loader_clear(loader_t* loader);
module_t* loader_lookup_module(loader_t* loader, char *module_name);
bool loader_is_module_loaded(loader_t* loader, char *module_name);
vm_address_t loader_lookup_address(loader_t* loader, char *module_name,
                                   vm_label_t label);
void loader_load_module(loader_t *loader, char* module_name,
                        satie_error_t* error);
void loader_pretty_print(loader_t* loader);
void loader_pretty_print_module(loader_t* loader, char* module_name);
void loader_unit_test(void);

#endif
