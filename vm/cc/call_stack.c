#define MUTE_LOG_DEBUG 1

#include <stdio.h>
#include "call_stack.h"
#include "log.h"

void call_stack_init(call_stack_t* call_stack, stack_t* initial_stack) {
                     //                     data_stack_t* data_stack) {
    call_stack->stack = initial_stack;
    call_stack->fp = 0;
    //call_stack->data_stack = data_stack;
}

void call_stack_free(call_stack_t* call_stack) {
    dynarray_clear(call_stack->stack);
}

size_t call_stack_length(call_stack_t* call_stack) {
    return dynarray_size(call_stack->stack);
}

void call_stack_push(call_stack_t* call_stack, vm_stack_value_t value) {
    dynarray_append(call_stack->stack, &value);
}

vm_stack_value_t call_stack_pop_string(call_stack_t*) {
    /*
    vm_stack_value_t* data_address = dynarray_pop(call_stack->stack);
    uint8_t* bytes = data_stack_peek(call_stack->data_stack, *data_address);
    vm_stack_value_t value =
        GET_VALUE(vm_stack_value_t, bytes + sizeof(vm_data_length_t));
    return value;
    */
    return 0;
}

vm_stack_value_t call_stack_pop(call_stack_t* call_stack) {
    vm_stack_value_t* value = dynarray_pop(call_stack->stack);
    return *value;
}

void call_stack_dup(call_stack_t* call_stack) {
    vm_stack_value_t* value =
        dynarray_element(call_stack->stack,
                         dynarray_size(call_stack->stack) - 1);
    dynarray_append(call_stack->stack, value);
}

void call_stack_swap(call_stack_t* call_stack) {
    if (dynarray_size(call_stack->stack) < 2) {
        LOG_ERROR("Stack underflow");
        return;
    }
    vm_stack_value_t* top = dynarray_pop(call_stack->stack);
    vm_stack_value_t* second_top = dynarray_pop(call_stack->stack);
    vm_stack_value_t temp = *second_top;
    dynarray_append(call_stack->stack, top);
    dynarray_append(call_stack->stack, &temp);
}

void call_stack_load(call_stack_t* call_stack) {
    vm_stack_value_t offset = call_stack_pop(call_stack);
    vm_stack_value_t* value =
        dynarray_element(call_stack->stack, call_stack->fp + offset);
    dynarray_append(call_stack->stack, value);
}

void call_stack_store(call_stack_t* call_stack) {
    vm_stack_value_t* value = dynarray_pop(call_stack->stack);
    vm_stack_value_t offset = call_stack_pop(call_stack);
    dynarray_setelement(call_stack->stack, call_stack->fp + offset, value);
}

void call_stack_binary_operation(call_stack_t* call_stack,
                                 vm_stack_value_t (*fun)(vm_stack_value_t,
                                                         vm_stack_value_t)) {
    vm_stack_value_t operand2 = call_stack_pop(call_stack);
    vm_stack_value_t operand1 = call_stack_pop(call_stack);
    call_stack_push(call_stack, fun(operand1, operand2));
}

void call_stack_print(call_stack_t* call_stack) {
    size_t i;
    for (i = 0; i < dynarray_size(call_stack->stack); i++) {
        vm_stack_value_t* value = dynarray_element(call_stack->stack, i);
        LOG_DEBUG("stack[%d] = %ld", i, *value);
    }
}

//
// Unit test
//

static vm_stack_value_t add(vm_stack_value_t a, vm_stack_value_t b) {
    return a + b;
}

void call_stack_unit_test(void) {
    call_stack_t call_stack;

    // Create initial empty stack
    stack_t stack;
    dynarray_init(&stack, NULL, 512, sizeof(vm_stack_value_t));

    // push, pop and length
    call_stack_init(&call_stack, &stack);
    call_stack_push(&call_stack, 1);
    LOG_ASSERT(call_stack_pop(&call_stack) == 1, "call_stack_pop");
    LOG_ASSERT(call_stack_length(&call_stack) == 0, "call_stack_length");

    // print
    call_stack_push(&call_stack, 1);
    call_stack_push(&call_stack, 2);
    call_stack_print(&call_stack);

    // swap
    call_stack_print(&call_stack);
    call_stack_swap(&call_stack);
    call_stack_print(&call_stack);
    LOG_ASSERT(call_stack_pop(&call_stack) == 1, "call_stack_pop");
    LOG_ASSERT(call_stack_pop(&call_stack) == 2, "call_stack_pop");

    // dup
    call_stack_push(&call_stack, 1);
    call_stack_push(&call_stack, 2);
    call_stack_dup(&call_stack);

    // binary_operation
    LOG_ASSERT(call_stack_pop(&call_stack) == 2, "call_stack_pop");
    LOG_ASSERT(call_stack_pop(&call_stack) == 2, "call_stack_pop");
    call_stack_push(&call_stack, 1);
    call_stack_push(&call_stack, 2);
    call_stack_binary_operation(&call_stack, add);
    LOG_ASSERT(call_stack_pop(&call_stack) == 3, "call_stack_pop");
    LOG_ASSERT(call_stack_length(&call_stack) == 1, "call_stack_length");
    call_stack_pop(&call_stack);

    // load
    call_stack_push(&call_stack, 5);
    call_stack_push(&call_stack, 6);
    call_stack_push(&call_stack, 7);
    call_stack_push(&call_stack, 8);
    call_stack_push(&call_stack, 1);
    call_stack_load(&call_stack);
    LOG_ASSERT(call_stack_pop(&call_stack) == 6, "call_stack_pop");

    // store
    call_stack_push(&call_stack, 2);
    call_stack_push(&call_stack, 42);
    call_stack_store(&call_stack);
    vm_stack_value_t* stored_value =
        dynarray_element(call_stack.stack, call_stack.fp + 2);
    LOG_ASSERT(*stored_value == 42, "call_stack_pop");

    LOG_INFO("call_stack_unit_test passed");
}
