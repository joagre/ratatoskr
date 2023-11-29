#ifndef __LOADER_H__
#define __LOADER_H__

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include "clib/lhash_kv.h"
#include "loader_module.h"

typedef struct {
    uint8_t* byte_code;
    size_t byte_code_size;
    const char* load_path;
    lhash_kv_t modules;
} loader_t;

typedef struct {
    bool success;
    int errno_value;
    char *error_message;
} loader_result_t;

void loader_init(loader_t* loader, const char* load_path);
loader_result_t loader_load_module(loader_t *loader, const char* module_name);
loader_result_t loader_generate_byte_code(module_t* module, FILE* file);
void purge_line(char *purged_line,  const char *line);

#endif
