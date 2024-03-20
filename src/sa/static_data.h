#ifndef SA_STATIC_DATA_H
#define SA_STATIC_DATA_H

#include <stdint.h>
#include <dynarr.h>
#include "vm.h"

typedef dynarray_t static_data_indices_t;

typedef struct {
    uint8_t* data;
    uint32_t size;
    uint32_t max_size;
    static_data_indices_t indices;
} static_data_t;

void static_data_init(static_data_t* data);
void static_data_clear(static_data_t* data);
vm_stack_value_t static_data_insert_string(static_data_t* data, char* string);
uint8_t* static_data_lookup(static_data_t* static_data, vm_stack_value_t index);
void static_data_pretty_print(static_data_t* static_data);

#endif
