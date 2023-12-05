#include <stdio.h>
#include "interpreter.h"
#include "log.h"
#include "pretty_print.h"
#include "scheduler.h"

// Forward declarations of local functions (alphabetical order)
static uint32_t spawn(scheduler_t* scheduler, vm_address_t address,
                      long* parameters, vm_arity_t arity);

void interpreter_init(interpreter_t *interpreter, interpreter_mode_t mode) {
    interpreter->mode = mode;
}

interpreter_result_t interpreter_run(scheduler_t *scheduler) {
    clock_t start_time = START_TIMER();
    uint32_t instructions_executed = 0;
    interpreter_result_t result;
    job_t* job = scheduler->running_job;

    while (true) {
#ifdef DEBUG
#ifndef MUTE_LOG_DEBUG
        if (scheduler->interpreter->mode == INTERPRETER_MODE_REGISTER) {
            fprintf(stderr, "%d: registers = ", job->jid);
            for (int i = 0; i < NUMBER_OF_REGISTERS; i++) {
                fprintf(stderr, "%ld ", job->registers[i]);
            }
        }
        fprintf(stderr, "%d: stack = ", job->jid);
        for (int i = 0; i < call_stack_length(&job->call_stack); i++) {
            vm_stack_value_t* value = DYN_ADDR(job->call_stack.stack_array, i);
            fprintf(stderr, "%ld ", *value);
        }
        fprintf(stderr, "=> %d:%d: ", job->jid, job->pc);
        print_instruction(&scheduler->loader->byte_code[job->pc]);
#endif
#endif

        uint32_t current_pc = job->pc;

        if (++job->pc > scheduler->loader->byte_code_size) {
            LOG_ABORT("Unexpected end of bytecode or invalid jump");
        }

        uint8_t* operands = &scheduler->loader->byte_code[job->pc];
        uint32_t size = 0;

        switch (scheduler->loader->byte_code[current_pc]) {
        // Register machine instructions
        case OPCODE_JMPRNZE: {
            vm_register_t register_ = GET_OPERAND(vm_register_t);
            vm_address_t address = GET_OPERAND(vm_address_t);
            if (job->registers[register_] != 0) {
                job->pc = address;
            } else {
                job->pc += size;
            }
            break;
        }
        case OPCODE_JMPRINGT: {
            vm_register_t register_ = GET_OPERAND(vm_register_t);
            vm_immediate_value_t immediate_value =
                GET_OPERAND(vm_immediate_value_t);
            vm_address_t address = GET_OPERAND(vm_address_t);
            if (!(job->registers[register_] > immediate_value)) {
                job->pc = address;
            } else {
                job->pc += size;
            }
            break;
        }
        case OPCODE_SUBRRI: {
            vm_register_t first_register = GET_OPERAND(vm_register_t);
            vm_register_t second_register = GET_OPERAND(vm_register_t);
            vm_immediate_value_t immediate_value =
                GET_OPERAND(vm_immediate_value_t);
            job->registers[first_register] =
                job->registers[second_register] - immediate_value;
            job->pc += size;
            break;
        }
        case OPCODE_SUBRSI: {
            vm_register_t register_ = GET_OPERAND(vm_register_t);
            vm_stack_offset_t stack_offset = GET_OPERAND(vm_stack_offset_t);
            vm_immediate_value_t immediate_value =
                GET_OPERAND(vm_immediate_value_t);
            job->registers[register_] =
                call_stack_array_get(&job->call_stack,
                                     job->call_stack.fp + stack_offset) -
                immediate_value;
            job->pc += size;
            break;
        }
        case OPCODE_ADDRRI: {
            vm_register_t first_register = GET_OPERAND(vm_register_t);
            vm_register_t second_register = GET_OPERAND(vm_register_t);
            vm_immediate_value_t immediate_value =
                GET_OPERAND(vm_immediate_value_t);
            job->registers[first_register] =
                job->registers[second_register] + immediate_value;
            job->pc += size;
            break;
        }
        case OPCODE_LOADRI: {
            vm_register_t register_ = GET_OPERAND(vm_register_t);
            vm_immediate_value_t immediate_value =
                GET_OPERAND(vm_immediate_value_t);
            job->registers[register_] = immediate_value;
            job->pc += size;
            break;
        }
        case OPCODE_PUSHR: {
            vm_register_t register_ = GET_OPERAND(vm_register_t);
            call_stack_push(&job->call_stack, job->registers[register_]);
            job->pc += size;
            break;
        }
        case OPCODE_LOADRS: {
            vm_register_t register_ = GET_OPERAND(vm_register_t);
            vm_stack_offset_t stack_offset = GET_OPERAND(vm_stack_offset_t);
            job->registers[register_] =
                call_stack_array_get(&job->call_stack,
                                     job->call_stack.fp + stack_offset);
            job->pc += size;
            break;
        }
        case OPCODE_LOADRR: {
            vm_register_t first_register = GET_OPERAND(vm_register_t);
            vm_register_t second_register = GET_OPERAND(vm_register_t);
            job->registers[first_register] = job->registers[second_register];
            job->pc += size;
            break;
        }
        case OPCODE_RCALL: {
            // Extract address to function
            vm_address_t address = GET_OPERAND(vm_address_t);
            // Push return address onto call stack
            call_stack_push(&job->call_stack, job->pc + sizeof(vm_address_t));
            // Push previous FP onto call stack
            call_stack_push(&job->call_stack, job->call_stack.fp);
            // Set FP to point at return address
            job->call_stack.fp = call_stack_length(&job->call_stack) - 2;
            // Jump to function address
            job->pc = address;
            break;
        }
        case OPCODE_RRET: {
            // Has call stack been exhausted?
            if (call_stack_length(&job->call_stack) == 2) {
                return INTERPRETER_RESULT_HALT;
            }
            vm_address_t return_address =
                call_stack_array_get(&job->call_stack, job->call_stack.fp);
            // Remember previous FP
            vm_stack_value_t previous_fp =
                call_stack_array_get(&job->call_stack, job->call_stack.fp + 1);
            // Remove call stack frame
            call_stack_array_set_size(&job->call_stack, job->call_stack.fp - 1);
            // Restore FP to previous FP
            job->call_stack.fp = previous_fp;
            // Jump to return address
            job->pc = return_address;
            break;
        }
        case OPCODE_JMP: {
            vm_address_t address = GET_OPERAND(vm_address_t);
            job->pc = address;
            break;
        }
        case OPCODE_POP: {
            call_stack_pop(&job->call_stack);
            break;
        }
        case OPCODE_SYS: {
            vm_system_call_t system_call = GET_OPERAND(vm_system_call_t);
            switch (system_call) {
            case SYSTEM_CALL_DISPLAY:
                vm_stack_value_t value = call_stack_pop(&job->call_stack);
                fprintf(stdout, "%ld\n", value);
                call_stack_push(&job->call_stack, 1);
                break;
            default:
                LOG_ABORT("Unknown system call");
            }
            break;
        }
        default:
            LOG_ABORT("Unknown opcode");
        }

        if (instructions_executed++ >= scheduler->check_after) {
            if (ELAPSED_TIME_MS(start_time) > scheduler->time_slice) {
                result = INTERPRETER_RESULT_TIMEOUT;
                break;
            }
            instructions_executed = 0;
        }
    }

    return result;
}

uint32_t interpreter_mspawn(scheduler_t *scheduler, char* module_name,
                            vm_label_t label, long* parameters,
                            vm_arity_t arity, satie_error_t* error) {
    // Ensure that module is loaded
    if (!loader_is_module_loaded(scheduler->loader, module_name)) {
        loader_load_module(scheduler->loader, module_name, error);
        if (error->failed) {
            return 0;
        }
    }

    vm_address_t address = loader_lookup_address(scheduler->loader, module_name, label);
    return spawn(scheduler, address, parameters, arity);
}

//
// Local functions (alphabetical order)
//

static uint32_t spawn(scheduler_t* scheduler, vm_address_t address,
                      long* parameters, vm_arity_t arity) {
    vm_stack_value_t return_address = -1;
    vm_stack_value_t fp = -1;

    // Prepare initial call stack depending on interpreter mode
    call_stack_array_t stack_array;
    call_stack_array_init(&stack_array);
    switch (scheduler->interpreter->mode) {
    case INTERPRETER_MODE_STACK:
        for (vm_arity_t i = 0; i < arity; i++) {
            call_stack_array_append(&stack_array, parameters[i]);
        }
        call_stack_array_append(&stack_array, arity);
        call_stack_array_append(&stack_array, return_address);
        call_stack_array_append(&stack_array, fp);
        break;
    case INTERPRETER_MODE_REGISTER:
        call_stack_array_append(&stack_array, return_address);
        call_stack_array_append(&stack_array, fp);
        break;
    }

    // Create job
    uint32_t jid = scheduler_next_jid();
    job_t* job = job_new(jid, address, &stack_array);

    // Set FP to point to arity
    job->call_stack.fp = call_stack_length(&job->call_stack) - 3;

    // Add job to scheduler
    scheduler_spawn(scheduler, job);

    return jid;
}
