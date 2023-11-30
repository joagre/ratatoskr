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

typedef struct {
    uint8_t* byte_code;
    size_t byte_code_size;
    size_t max_byte_code_size;
    const char* load_path;
    lhash_kv_t modules;
} loader_t;

void loader_init(loader_t* loader, const char* load_path);
void loader_load_module(loader_t *loader, const char* module_name,
                        satie_error_t* satie_error);

#endif
