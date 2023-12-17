#ifndef __DATA_STACK_H__
#define __DATA_STACK_H__

#include "clib/dynarr.h"
#include "vm.h"

typedef dynarray_t data_stack_array_t;

typedef struct {
    data_stack_array_t array;
    vm_stack_value_t fp;
} data_stack_t;

void data_stack_init(data_stack_t* data_stack);
void data_stack_free(data_stack_t* data_stack);
size_t data_stack_length(data_stack_t* data_stack);
vm_data_length_t data_stack_push(data_stack_t* data_stack, uint8_t* bytes,
                                 vm_address_t* data_address);
uint8_t* data_stack_peek(data_stack_t* data_stack, vm_address_t data_address);

#endif
