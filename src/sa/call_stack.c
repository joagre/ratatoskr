#define MUTE_LOG_DEBUG 1

#include "call_stack.h"
#include "log.h"

void call_stack_init(call_stack_t* call_stack) {
    dynarray_init(&call_stack->array, NULL, INITIAL_CALL_STACK_SIZE,
                  sizeof(vm_stack_value_t));
    call_stack->fp = 0;
}

void call_stack_clear(call_stack_t* call_stack) {
    dynarray_clear(&call_stack->array);
}

size_t call_stack_size(call_stack_t* call_stack) {
    return dynarray_size(&call_stack->array);
}

vm_stack_value_t call_stack_get(call_stack_t* call_stack, size_t index) {
    vm_stack_value_t* stack_value = DYN_ADDR(&call_stack->array, index);
    return *stack_value;
}

void call_stack_push(call_stack_t* call_stack, vm_stack_value_t value) {
    dynarray_append(&call_stack->array, &value);
}

vm_stack_value_t call_stack_pop(call_stack_t* call_stack) {
    vm_stack_value_t* value = dynarray_pop(&call_stack->array);
    return *value;
}

void call_stack_set_size(call_stack_t* call_stack, size_t size) {
    LOG_ASSERT(size <= call_stack->array.capacity,
               "Call stack overflow: %zu > %zu",
               size, call_stack->array.capacity);
    call_stack->array.size = size;
}

void call_stack_dup(call_stack_t* call_stack) {
    vm_stack_value_t* value =
        dynarray_element(&call_stack->array,
                         dynarray_size(&call_stack->array) - 1);
    dynarray_append(&call_stack->array, value);
}

void call_stack_swap(call_stack_t* call_stack) {
    if (dynarray_size(&call_stack->array) < 2) {
        LOG_ERROR("Stack underflow");
        return;
    }
    vm_stack_value_t* top = dynarray_pop(&call_stack->array);
    vm_stack_value_t* second_top = dynarray_pop(&call_stack->array);
    vm_stack_value_t temp = *second_top;
    dynarray_append(&call_stack->array, top);
    dynarray_append(&call_stack->array, &temp);
}

void call_stack_load(call_stack_t* call_stack) {
    vm_stack_value_t offset = call_stack_pop(call_stack);
    vm_stack_value_t* value =
        dynarray_element(&call_stack->array, call_stack->fp + offset);
    dynarray_append(&call_stack->array, value);
}

void call_stack_store(call_stack_t* call_stack) {
    vm_stack_value_t* value = dynarray_pop(&call_stack->array);
    vm_stack_value_t offset = call_stack_pop(call_stack);
    dynarray_setelement(&call_stack->array, call_stack->fp + offset, value);
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
    for (i = 0; i < dynarray_size(&call_stack->array); i++) {
        vm_stack_value_t* value = dynarray_element(&call_stack->array, i);
        printf("stack[%ld] = %ld\n", i, *value);
    }
}

//
// Unit test
//

static vm_stack_value_t add(vm_stack_value_t a, vm_stack_value_t b) {
    return a + b;
}

void call_stack_unit_test(void) {
    // Create call stack
    call_stack_t call_stack;
    call_stack_init(&call_stack);

    // push, pop, length
    call_stack_push(&call_stack, 1);
    LOG_ASSERT(call_stack_pop(&call_stack) == 1, "call_stack_pop");
    LOG_ASSERT(call_stack_size(&call_stack) == 0, "call_stack_size");

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
    LOG_ASSERT(call_stack_size(&call_stack) == 1, "call_stack_size");
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
    vm_stack_value_t stored_value = call_stack_get(&call_stack, call_stack.fp + 2);
    LOG_ASSERT(stored_value == 42, "call_stack_pop");

    LOG_INFO("call_stack_unit_test passed");
}
