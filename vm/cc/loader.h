#ifndef __LOADER_H__
#define __LOADER_H__

#include <stdint.h>
#include <stdio.h>
#include "clib/lhash_kv.h"
#include "module.h"
#include "satie.h"

typedef struct {
    uint8_t* byte_code;
    size_t byte_code_size;
    const char* load_path;
    lhash_kv_t modules;
} loader_t;

void loader_init(loader_t* loader, const char* load_path);
void loader_load_module(loader_t *loader, const char* module_name,
                        satie_error_t* satie_error);
void loader_generate_byte_code(loader_t *loader, module_t* module, FILE* file,
                               satie_error_t* satie_error);

#endif
