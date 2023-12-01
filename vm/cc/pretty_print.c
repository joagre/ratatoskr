#include <stdio.h>
#include "vm.h"

uint32_t print_instruction(uint8_t* bytes) {
    // These variables are required by the GET_OPERAND macro
    uint8_t* operands = bytes + OPCODE_SIZE;
    uint32_t size = 0;

    switch (*bytes) {
    // Register machine instructions
    case OPCODE_JMPRNZE: {
        vm_register_t register_ = GET_OPERAND(vm_register_t);
        vm_address_t address = GET_OPERAND(vm_address_t);
        fprintf(stdout, "jmprnze r%d %d", register_, address);
        return size;
    }
    case OPCODE_JMPRINGT: {
        vm_register_t register_value = GET_OPERAND(vm_register_t);
        vm_immediate_value_t immediate_value = GET_OPERAND(vm_immediate_value_t);
        vm_address_t address = GET_OPERAND(vm_address_t);
        fprintf(stdout, "jmpringt r%d #%ld %d", register_value, immediate_value,
                address);
        return size;
    }
    case OPCODE_SUBRRI: {
        vm_register_t first_register = GET_OPERAND(vm_register_t);
        vm_register_t second_register = GET_OPERAND(vm_register_t);
        vm_immediate_value_t immediate_value = GET_OPERAND(vm_immediate_value_t);
        fprintf(stdout, "subrri r%d r%d #%ld", first_register, second_register,
                immediate_value);
        return size;
    }
    case OPCODE_SUBRSI: {
        vm_register_t register_value = GET_OPERAND(vm_register_t);
        vm_stack_offset_t stack_offset = GET_OPERAND(vm_stack_offset_t);
        vm_immediate_value_t immediate_value = GET_OPERAND(vm_immediate_value_t);
        fprintf(stdout, "subrsi r%d @%d #%ld", register_value, stack_offset,
                immediate_value);
        return size;
    }
    case OPCODE_ADDRRI: {
        vm_register_t first_register = GET_OPERAND(vm_register_t);
        vm_register_t second_register = GET_OPERAND(vm_register_t);
        vm_immediate_value_t immediate_value = GET_OPERAND(vm_immediate_value_t);
        fprintf(stdout, "addrri r%d r%d #%ld", first_register, second_register,
                immediate_value);
        return size;
    }
    case OPCODE_LOADRI: {
        vm_register_t register_ = GET_OPERAND(vm_register_t);
        vm_immediate_value_t immediate_value = GET_OPERAND(vm_immediate_value_t);
        fprintf(stdout, "loadri r%d #%ld", register_, immediate_value);
        return size;
    }
    case OPCODE_PUSHR: {
        vm_register_t register_ = GET_OPERAND(vm_register_t);
        fprintf(stdout, "pushr r%d", register_);
        return size;
    }
    case OPCODE_LOADRS: {
        vm_register_t register_ = GET_OPERAND(vm_register_t);
        vm_stack_offset_t stack_offset = GET_OPERAND(vm_stack_offset_t);
        fprintf(stdout, "loadrs r%d @%d", register_, stack_offset);
        return size;
    }
    case OPCODE_LOADRR: {
        vm_register_t first_register = GET_OPERAND(vm_register_t);
        vm_register_t second_register = GET_OPERAND(vm_register_t);
        fprintf(stdout, "loadrr r%d r%d", first_register, second_register);
        return size;
    }
    case OPCODE_RCALL: {
        vm_address_t address = GET_OPERAND(vm_address_t);
        fprintf(stdout, "rcall %d", address);
        return size;
    }
    case OPCODE_JMP: {
        vm_address_t address = GET_OPERAND(vm_address_t);
        fprintf(stdout, "jmp %d", address);
        return size;
    // Stack machine instructions
    }
    case OPCODE_PUSH: {
        vm_stack_value_t stack_value = GET_OPERAND(vm_stack_value_t);
        fprintf(stdout, "push %ld", stack_value);
        return size;
    }
    case OPCODE_PUSHS: {
        vm_data_length_t data_length = GET_OPERAND(vm_data_length_t);
        uint8_t* byte_string = operands + size;
        fprintf(stdout, "pushs \"%s\"", byte_string);
        return size + data_length;
    }
    case OPCODE_JUMP: {
        vm_address_t address = GET_OPERAND(vm_address_t);
        fprintf(stdout, "jump %d", address);
        return size;
    }
    case OPCODE_CJUMP: {
        vm_address_t address = GET_OPERAND(vm_address_t);
        fprintf(stdout, "cjump %d", address);
        return size;
    }
    case OPCODE_CALL: {
        vm_address_t address = GET_OPERAND(vm_address_t);
        vm_arity_t arity = GET_OPERAND(vm_arity_t);
        fprintf(stdout, "call %d %d", address, arity);
        return size;
    }
    case OPCODE_RET: {
        vm_return_mode_t return_mode = GET_OPERAND(vm_return_mode_t);
        if (return_mode == RETURN_MODE_COPY) {
            fprintf(stdout, "ret copy");
        } else {
            fprintf(stdout, "ret");
        }
        return size;
    }
    case OPCODE_SYS: {
        vm_system_call_t system_call = GET_OPERAND(vm_system_call_t);
        fprintf(stdout, "sys %d", system_call);
        return size;
    }
    case OPCODE_SPAWN: {
        vm_address_t address = GET_OPERAND(vm_address_t);
        vm_arity_t arity = GET_OPERAND(vm_arity_t);
        fprintf(stdout, "spawn %d %d", address, arity);
        return size;
    }
    default:
        fprintf(stderr, "Unknown opcode: %d", bytes[0]);
    }

    return 0;
}
