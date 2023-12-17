#include "data_stack.h"

void data_stack_init(data_stack_t* data_stack) {
    dynarray_init(&data_stack->array, NULL, 512, sizeof(uint8_t));
    data_stack->fp = 0;
}

void data_stack_free(data_stack_t* data_stack) {
    dynarray_clear(&data_stack->array);
}

size_t data_stack_length(data_stack_t* data_stack) {
    return dynarray_size(&data_stack->array);
}

vm_data_length_t data_stack_push(data_stack_t* data_stack, uint8_t* bytes,
                                 vm_address_t* data_address) {
    vm_data_length_t length = GET_VALUE(vm_data_length_t, bytes);
    *data_address = dynarray_size(&data_stack->array);
    for (size_t i = 0; i < length; i++) {
        dynarray_append(&data_stack->array,
                        bytes + sizeof(vm_data_length_t) + i);
    }
    return length;
}

uint8_t* data_stack_peek(data_stack_t* data_stack, vm_address_t data_address) {
    return dynarray_element(&data_stack->array, data_address);
}
