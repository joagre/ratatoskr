#include <stdio.h>
#include <time.h>
#include "interpreter.h"
#include "call_stack.h"
#include "job.h"
#include "scheduler.h"
#include "loader.h"
#include "log.h"
#include "pretty_print.h"

void interpreter_init(interpreter_t *interpreter, loader_t* loader,
                      interpreter_mode_t mode) {
    interpreter->loader = loader;
    interpreter->mode = mode;
}

interpreter_result_t run(interpreter_t *interpreter, scheduler_t *scheduler,
                         job_t* job, uint32_t time_slice,
                         uint16_t check_after) {
    clock_t start_time = START_TIMER();
    uint32_t instructions_executed = 0;
    interpreter_result_t interpreter_result;

    while (true) {
#ifdef DEBUG
#ifndef MUTE_LOG_DEBUG
        if (interpreter->mode == INTERPRETER_MODE_REGISTER) {
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
        print_instruction(&interpreter->loader->byte_code[job->pc]);
#endif
#endif

        uint32_t current_pc = job->pc;

        if (++job->pc > interpreter->loader->byte_code_size) {
            LOG_ABORT("Unexpected end of bytecode or invalid jump");
        }

        uint8_t* operands = &interpreter->loader->byte_code[job->pc];
        uint32_t size = 0;

        switch (interpreter->loader->byte_code[current_pc]) {
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
        }
        default:
            LOG_ABORT("Unknown opcode");
        }

        if (instructions_executed++ >= check_after) {
            if (ELAPSED_TIME_MS(start_time) > time_slice) {
                interpreter_result = INTERPRETER_RESULT_TIMEOUT;
                break;
            }
            instructions_executed = 0;
        }
    }

    return interpreter_result;
}
