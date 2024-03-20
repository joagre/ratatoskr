#ifndef SA_CALL_STACK_H
#define SA_CALL_STACK_H

#include <dynarr.h>
#include "vm.h"
#include "log.h"

typedef dynarray_t call_stack_array_t;

typedef struct {
    call_stack_array_t array;
    vm_stack_value_t fp;
} call_stack_t;

void call_stack_init(call_stack_t* call_stack);
void call_stack_clear(call_stack_t* call_stack);
size_t call_stack_size(call_stack_t* call_stack);
vm_stack_value_t call_stack_get(call_stack_t* call_stack, size_t index);
void call_stack_push(call_stack_t* call_stack, vm_stack_value_t value);
vm_stack_value_t call_stack_pop(call_stack_t* call_stack);
void call_stack_set_size(call_stack_t* call_stack, size_t size);
void call_stack_dup(call_stack_t* call_stack);
void call_stack_swap(call_stack_t* call_stack);
void call_stack_load(call_stack_t* call_stack);
void call_stack_store(call_stack_t* call_stack);
void call_stack_binary_operation(call_stack_t* call_stack,
                                 vm_stack_value_t (*fun)(vm_stack_value_t,
                                                         vm_stack_value_t));
void call_stack_print(call_stack_t* call_stack);
void call_stack_unit_test(void);

/*
static inline size_t call_stack_size(call_stack_t* call_stack) {
    return dynarray_size(&call_stack->array);
}

static inline vm_stack_value_t call_stack_get(call_stack_t* call_stack, size_t index) {
    vm_stack_value_t* stack_value = DYN_ADDR(&call_stack->array, index);
    return *stack_value;
}

static inline void call_stack_push(call_stack_t* call_stack, vm_stack_value_t value) {
    dynarray_append(&call_stack->array, &value);
}

static inline vm_stack_value_t call_stack_pop(call_stack_t* call_stack) {
    vm_stack_value_t* value = dynarray_pop(&call_stack->array);
    return *value;
}

static inline void call_stack_set_size(call_stack_t* call_stack, size_t size) {
    LOG_ASSERT(size <= call_stack->array.capacity,
               "Call stack overflow: %zu > %zu",
               size, call_stack->array.capacity);
    call_stack->array.size = size;
}
*/

#endif
