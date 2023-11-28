#ifndef __LOADER_H__
#define __LOADER_H__

#include <stdint.h>
#include "clib/lhash_kv.h"

typedef struct {
    uint8_t* byte_code;
    size_t byte_code_size;
    const char* load_path;
    lhash_kv_t modules;
} loader_t;

void loader_init(loader_t* loader, const char* load_path);

#endif
