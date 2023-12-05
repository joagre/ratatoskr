#ifndef __CALL_STACK_H__
#define __CALL_STACK_H__

#include "data_stack.h"
#include "vm.h"

typedef dynarray_t call_stack_array_t;

typedef struct {
    call_stack_array_t* stack_array;
    vm_stack_value_t fp;
    data_stack_t* data_stack;
} call_stack_t;

static inline vm_stack_value_t call_stack_array_get(call_stack_t* call_stack, size_t index) {
    vm_stack_value_t* stack_value = DYN_ADDR(call_stack->stack_array, index);
    return *stack_value;
}

static inline void call_stack_array_set_size(call_stack_t* call_stack, size_t size) {
    call_stack->stack_array->size = size;
}

void call_stack_init(call_stack_t* call_stack, call_stack_array_t* stack_array,
                     data_stack_t* data_stack);
void call_stack_free(call_stack_t* call_stack);
void call_stack_array_init(call_stack_array_t* stack_array);
void call_stack_array_free(call_stack_array_t* stack_array);
void call_stack_array_append(call_stack_array_t* stack_array,
                             vm_stack_value_t value);
size_t call_stack_length(call_stack_t* call_stack);
void call_stack_push(call_stack_t* call_stack, vm_stack_value_t value);
char* call_stack_pop_string(call_stack_t* call_stack);
vm_stack_value_t call_stack_pop(call_stack_t* call_stack);
void call_stack_dup(call_stack_t* call_stack);
void call_stack_swap(call_stack_t* call_stack);
void call_stack_load(call_stack_t* call_stack);
void call_stack_store(call_stack_t* call_stack);
void call_stack_binary_operation(call_stack_t* call_stack,
                                 vm_stack_value_t (*fun)(vm_stack_value_t,
                                                         vm_stack_value_t));
void call_stack_print(call_stack_t* call_stack);
void call_stack_unit_test(void);

#endif
