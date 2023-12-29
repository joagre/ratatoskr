#define MUTE_LOG_DEBUG

#include <stdio.h>
#include <log.h>
#include "pretty_print.h"
#include "interpreter.h"
#include "scheduler.h"
#include "module.h"

// Forward declarations of local functions (alphabetical order)
static void call(job_t* job, vm_address_t address, size_t size_of_operands);
static uint32_t spawn(scheduler_t* scheduler, vm_address_t address,
		      vm_stack_value_t* parameters, uint8_t arity);

void interpreter_init(interpreter_t *interpreter, loader_t* loader) {
    interpreter->loader = loader;
}

void interpreter_clear(interpreter_t *interpreter) {
    (void*)interpreter;
}

interpreter_result_t interpreter_run(interpreter_t* interpreter,
				     scheduler_t *scheduler) {
    clock_t start_time = START_TIMER();
    uint32_t instructions_executed = 0;
    interpreter_result_t result;
    job_t* job = scheduler->running_job;

    while (true) {
#ifdef DEBUG
#ifndef MUTE_LOG_DEBUG
        fprintf(stderr, "%d: registers = [", job->jid);
        for (int i = 0; i < 8; i++) {
            fprintf(stderr, "%ld", job->registers[i]);
            if (i < 7) {
                fprintf(stderr, ", ");
            }
        }
        fprintf(stderr, "]\n");
        fprintf(stderr, "%d: stack = [", job->jid);
        size_t n = call_stack_size(&job->call_stack);
        for (size_t i = 0; i < n; i++) {
            vm_stack_value_t value = call_stack_get(&job->call_stack, i);
            if (i < n - 1) {
                fprintf(stderr, "%ld, ", value);
            } else {
                fprintf(stderr, "%ld", value);
            }
        }
        fprintf(stderr, "]\n");
        fprintf(stderr, "==> %d:%d: ", job->jid, job->pc);
        print_instruction(&interpreter->loader->bytecode[job->pc],
			  &interpreter->loader->static_data);
#endif
#endif

        uint32_t current_pc = job->pc;

        if (++job->pc > interpreter->loader->bytecode_size) {
            LOG_ABORT("Unexpected end of bytecode or invalid jump");
        }

        // These variables are required by the GET_OPERAND macro
        uint8_t* operands = &interpreter->loader->bytecode[job->pc];
        uint32_t size = 0;

        switch (interpreter->loader->bytecode[current_pc]) {
	    // Register machine instructions
	    case OPCODE_JMPRINEQ: {
		vm_register_t register_ = GET_OPERAND(vm_register_t);
		vm_immediate_value_t value = GET_OPERAND(vm_immediate_value_t);
		vm_address_t address = GET_OPERAND(vm_address_t);
		if (job->registers[register_] != value) {
		    job->pc = address;
		} else {
		    job->pc += size;
		}
		break;
	    }
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
		vm_immediate_value_t value = GET_OPERAND(vm_immediate_value_t);
		vm_address_t address = GET_OPERAND(vm_address_t);
		if (!(job->registers[register_] > value)) {
		    job->pc = address;
		} else {
		    job->pc += size;
		}
		break;
	    }
	    case OPCODE_SUBRRI: {
		vm_register_t first_register = GET_OPERAND(vm_register_t);
		vm_register_t second_register = GET_OPERAND(vm_register_t);
		vm_immediate_value_t value = GET_OPERAND(vm_immediate_value_t);
		job->registers[first_register] =
		    job->registers[second_register] - value;
		job->pc += size;
		break;
	    }
	    case OPCODE_SUBRSI: {
		vm_register_t register_ = GET_OPERAND(vm_register_t);
		vm_stack_offset_t stack_offset = GET_OPERAND(vm_stack_offset_t);
		vm_immediate_value_t value = GET_OPERAND(vm_immediate_value_t);
		job->registers[register_] =
		    call_stack_get(&job->call_stack,
				   job->call_stack.fp + stack_offset) - value;
		job->pc += size;
		break;
	    }
	    case OPCODE_ADDRRI: {
		vm_register_t first_register = GET_OPERAND(vm_register_t);
		vm_register_t second_register = GET_OPERAND(vm_register_t);
		vm_immediate_value_t value = GET_OPERAND(vm_immediate_value_t);
		job->registers[first_register] =
		    job->registers[second_register] + value;
		job->pc += size;
		break;
	    }
	    case OPCODE_LOADRI: {
		vm_register_t register_ = GET_OPERAND(vm_register_t);
		vm_immediate_value_t value = GET_OPERAND(vm_immediate_value_t);
		job->registers[register_] = value;
		job->pc += size;
		break;
	    }
	    case OPCODE_PUSHI: {
		vm_immediate_value_t value = GET_OPERAND(vm_immediate_value_t);
		call_stack_push(&job->call_stack, value);
		job->pc += size;
		break;
	    }
	    case OPCODE_PUSHR: {
		vm_register_t register_ = GET_OPERAND(vm_register_t);
		call_stack_push(&job->call_stack, job->registers[register_]);
		job->pc += size;
		break;
	    }
	    case OPCODE_PUSHSTR: {
		vm_stack_value_t value = GET_OPERAND(vm_stack_value_t);
		call_stack_push(&job->call_stack, value);
		job->pc += size;
		break;
	    }
	    case OPCODE_POP: {
		call_stack_pop(&job->call_stack);
		break;
	    }
	    case OPCODE_POPR: {
		vm_register_t register_ = GET_OPERAND(vm_register_t);
		vm_stack_value_t value = call_stack_pop(&job->call_stack);
		job->registers[register_] = value;
		job->pc += size;
		break;
	    }
	    case OPCODE_LOADRS: {
		vm_register_t register_ = GET_OPERAND(vm_register_t);
		vm_stack_offset_t stack_offset = GET_OPERAND(vm_stack_offset_t);
		job->registers[register_] =
		    call_stack_get(&job->call_stack,
				   job->call_stack.fp + stack_offset);
		job->pc += size;
		break;
	    }
	    case OPCODE_LOADRR: {
		vm_register_t first_register = GET_OPERAND(vm_register_t);
		vm_register_t second_register = GET_OPERAND(vm_register_t);
		job->registers[first_register] =
		    job->registers[second_register];
		job->pc += size;
		break;
	    }
	    case OPCODE_CALL: {
		vm_address_t address = GET_OPERAND(vm_address_t);
		call(job, address, size);
		break;
	    }
	    case OPCODE_RET: {
		// Remember return address
		vm_stack_value_t return_address =
		    call_stack_get(&job->call_stack, job->call_stack.fp);
		// Remember previous FP
		vm_stack_value_t previous_fp =
		    call_stack_get(&job->call_stack, job->call_stack.fp + 1);
		// Remove call stack frame
		call_stack_set_size(&job->call_stack, job->call_stack.fp);

		// Has call stack been exhausted?
		if (return_address == 0 && previous_fp == 0) {
		    return INTERPRETER_RESULT_HALT;
		}
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
	    case OPCODE_MULRRR: {
		vm_register_t first_register = GET_OPERAND(vm_register_t);
		vm_register_t second_register = GET_OPERAND(vm_register_t);
		vm_register_t third_register = GET_OPERAND(vm_register_t);
		job->registers[first_register] =
		    job->registers[second_register] *
		    job->registers[third_register];
		job->pc += size;
		break;
	    }
	    case OPCODE_SYS: {
		vm_system_call_t system_call = GET_OPERAND(vm_system_call_t);
		switch (system_call) {
		    case SYSTEM_CALL_SELF:
			call_stack_push(&job->call_stack, job->jid);
			break;
		    case SYSTEM_CALL_SEND:
			vm_stack_value_t jid = job->registers[1];
			vm_stack_value_t value = job->registers[2];
			satie_error_t error;
			scheduler_send_message(scheduler, jid, value, &error);
			if (error.failed) {
			    LOG_SATIE_ERROR(LOG_LEVEL_ERROR, &error);
			    return INTERPRETER_RESULT_HALT;
			}
			break;
		    case SYSTEM_CALL_RECV:
			if (mailbox_is_empty(&job->mailbox)) {
			    --job->pc;
			    return INTERPRETER_RESULT_RECV;
			}
			job->registers[0] = mailbox_dequeue(&job->mailbox);
			break;
		    case SYSTEM_CALL_PRINTLN:
			fprintf(stderr, "%s\n",
				(char*)static_data_lookup(
				    &interpreter->loader->static_data,
				    job->registers[1]));
			break;
		    case SYSTEM_CALL_DISPLAY:
			fprintf(stderr, "%ld\n", job->registers[1]);
			break;
		    case SYSTEM_CALL_EXIT:
			return INTERPRETER_RESULT_EXIT;
		    default:
			LOG_ABORT("Unknown system call");
		}
		job->pc += size;
		break;
	    }
            case OPCODE_MCALL: {
		vm_stack_value_t static_data_index =
		    call_stack_pop(&job->call_stack);
		LOG_DEBUG("static_data_index = %ld", static_data_index);
		char* module_name =
		    (char*)static_data_lookup(&interpreter->loader->static_data,
					      static_data_index);
		LOG_DEBUG("module_name = %s", module_name);
		vm_label_t label = call_stack_pop(&job->call_stack);
		LOG_DEBUG("label = %d", label);
                // Ensure that module is loaded
		satie_error_t error;
		if (!loader_is_module_loaded(interpreter->loader,
					     module_name)) {
		    loader_load_module(interpreter->loader, module_name,
				       &error);
		    if (error.failed) {
			LOG_PANIC("Failed to load module %s", module_name);
			return INTERPRETER_RESULT_HALT;
		    }
		}
		vm_address_t address =
		    loader_lookup_address(interpreter->loader, module_name,
					  label);
		call(job, address, 0);
		break;
	    }
	    case OPCODE_SPAWN: {
		vm_address_t address = GET_OPERAND(vm_address_t);
		vm_stack_value_t arity = call_stack_pop(&job->call_stack);
		LOG_DEBUG("address = %d", address);
		job->registers[0] = spawn(scheduler, address, NULL, arity);
                job->pc += size;
                break;
	    }
	    case OPCODE_MSPAWN: {
		vm_stack_value_t static_data_index =
		    call_stack_pop(&job->call_stack);
		char* module_name = (char*)static_data_lookup(
		    &interpreter->loader->static_data, static_data_index);
		vm_label_t label = call_stack_pop(&job->call_stack);
		vm_stack_value_t arity = call_stack_pop(&job->call_stack);
		satie_error_t error;
		uint32_t jid = interpreter_mspawn(interpreter, scheduler,
						  module_name, label, NULL,
						  arity, &error);
		if (error.failed) {
		    LOG_PANIC("Failed to spawn module %s", module_name);
		    return INTERPRETER_RESULT_HALT;
		}
		job->registers[0] = jid;
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

uint32_t interpreter_mspawn(interpreter_t* interpreter, scheduler_t *scheduler,
			    char* module_name, vm_label_t label,
			    vm_stack_value_t* parameters, uint8_t arity,
			    satie_error_t* error) {
    // Ensure that module is loaded
    if (!loader_is_module_loaded(interpreter->loader, module_name)) {
        loader_load_module(interpreter->loader, module_name, error);
        if (error->failed) {
            return 0;
        }
    }
    vm_address_t address =
	loader_lookup_address(interpreter->loader, module_name, label);
    return spawn(scheduler, address, parameters, arity);
}

//
// Local functions (alphabetical order)
//

void call(job_t* job, vm_address_t address, size_t size_of_operands) {
    // Push return address onto call stack
    call_stack_push(&job->call_stack, job->pc + size_of_operands);
    // Push previous FP onto call stack
    call_stack_push(&job->call_stack, job->call_stack.fp);
    // Set FP to point at return address
    job->call_stack.fp = call_stack_size(&job->call_stack) - 2;
    // Jump to function address
    job->pc = address;
}

static uint32_t spawn(scheduler_t* scheduler, vm_address_t address,
                      vm_stack_value_t* parameters, uint8_t arity) {
    uint32_t jid = scheduler_next_jid();
    job_t* job =
	job_new(scheduler->running_job, jid, address, parameters, arity);
    scheduler_spawn(scheduler, job);
    return jid;
}
