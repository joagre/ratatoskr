#include <stdio.h>
#include "vm.h"
#include "pretty_print.h"

uint32_t print_instruction(uint8_t* bytes, static_data_t* static_data) {
    // These variables are required by the GET_OPERAND macro
    uint8_t* operands = bytes + OPCODE_SIZE;
    uint32_t size = 0;

    switch (*bytes) {
	case OPCODE_JMPRINEQ: {
	    vm_register_t register_ = GET_OPERAND(vm_register_t);
	    vm_immediate_value_t value = GET_OPERAND(vm_immediate_value_t);
	    vm_address_t address = GET_OPERAND(vm_address_t);
	    fprintf(stderr, "jmprineq r%d #%ld %d\n", register_, value,
		    address);
	    return size;
	}
	case OPCODE_JMPRNZE: {
	    vm_register_t register_ = GET_OPERAND(vm_register_t);
	    vm_address_t address = GET_OPERAND(vm_address_t);
	    fprintf(stderr, "jmprnze r%d %d\n", register_, address);
	    return size;
	}
	case OPCODE_JMPRINGT: {
	    vm_register_t register_value = GET_OPERAND(vm_register_t);
	    vm_immediate_value_t immediate_value =
		GET_OPERAND(vm_immediate_value_t);
	    vm_address_t address = GET_OPERAND(vm_address_t);
	    fprintf(stderr, "jmpringt r%d #%ld %d\n", register_value,
		    immediate_value, address);
	    return size;
	}
	case OPCODE_SUBRRI: {
	    vm_register_t first_register = GET_OPERAND(vm_register_t);
	    vm_register_t second_register = GET_OPERAND(vm_register_t);
	    vm_immediate_value_t immediate_value =
		GET_OPERAND(vm_immediate_value_t);
	    fprintf(stderr, "subrri r%d r%d #%ld\n", first_register,
		    second_register, immediate_value);
	    return size;
	}
	case OPCODE_SUBRSI: {
	    vm_register_t register_value = GET_OPERAND(vm_register_t);
	    vm_stack_offset_t stack_offset = GET_OPERAND(vm_stack_offset_t);
	    vm_immediate_value_t immediate_value =
		GET_OPERAND(vm_immediate_value_t);
	    fprintf(stderr, "subrsi r%d @%d #%ld\n", register_value,
		    stack_offset,
		    immediate_value);
	    return size;
	}
	case OPCODE_ADDRRI: {
	    vm_register_t first_register = GET_OPERAND(vm_register_t);
	    vm_register_t second_register = GET_OPERAND(vm_register_t);
	    vm_immediate_value_t immediate_value =
		GET_OPERAND(vm_immediate_value_t);
	    fprintf(stderr, "addrri r%d r%d #%ld\n", first_register,
		    second_register,
		    immediate_value);
	    return size;
	}
	case OPCODE_LOADRI: {
	    vm_register_t register_ = GET_OPERAND(vm_register_t);
	    vm_immediate_value_t immediate_value =
		GET_OPERAND(vm_immediate_value_t);
	    fprintf(stderr, "loadri r%d #%ld\n", register_, immediate_value);
	    return size;
	}
	case OPCODE_PUSHI: {
	    vm_immediate_value_t value = GET_OPERAND(vm_immediate_value_t);
	    fprintf(stderr, "pushi #%ld\n", value);
	    return size;
	}
	case OPCODE_PUSHR: {
	    vm_register_t register_ = GET_OPERAND(vm_register_t);
	    fprintf(stderr, "pushr r%d\n", register_);
	    return size;
	}
	case OPCODE_PUSHSTR: {
	    vm_stack_value_t index = GET_OPERAND(vm_stack_value_t);
	    if (static_data != NULL) {
		fprintf(stderr, "pushstr \"%s\"\n",
			(char*)static_data_lookup(static_data, index));
	    } else {
		fprintf(stderr, "pushstr %ld\n", index);
	    }
	    return size;
	}
	case OPCODE_POPR: {
	    vm_register_t register_ = GET_OPERAND(vm_register_t);
	    fprintf(stderr, "popr r%d\n", register_);
	    return size;
	}
	case OPCODE_LOADRS: {
	    vm_register_t register_ = GET_OPERAND(vm_register_t);
	    vm_stack_offset_t stack_offset = GET_OPERAND(vm_stack_offset_t);
	    fprintf(stderr, "loadrs r%d @%d\n", register_, stack_offset);
	    return size;
	}
	case OPCODE_LOADRR: {
	    vm_register_t first_register = GET_OPERAND(vm_register_t);
	    vm_register_t second_register = GET_OPERAND(vm_register_t);
	    fprintf(stderr, "loadrr r%d r%d\n", first_register,
		    second_register);
	    return size;
	}
	case OPCODE_CALL: {
	    vm_address_t address = GET_OPERAND(vm_address_t);
	    fprintf(stderr, "call %d\n", address);
	    return size;
	}
	case OPCODE_JMP: {
	    vm_address_t address = GET_OPERAND(vm_address_t);
	    fprintf(stderr, "jmp %d\n", address);
	    return size;
	}
	case OPCODE_MULRRR: {
	    vm_register_t first_register = GET_OPERAND(vm_register_t);
	    vm_register_t second_register = GET_OPERAND(vm_register_t);
	    vm_register_t third_register = GET_OPERAND(vm_register_t);
	    fprintf(stderr, "mulrrr r%d r%d r%d\n", first_register,
		    second_register, third_register);
	    return size;
	}
	case OPCODE_SYS: {
	    vm_system_call_t system_call = GET_OPERAND(vm_system_call_t);
	    fprintf(stderr, "sys %d\n", system_call);
	    return size;
	}
	case OPCODE_SPAWN: {
	    vm_address_t address = GET_OPERAND(vm_address_t);
	    fprintf(stderr, "spawn %d\n", address);
	    return size;
	}
	default:
	    opcode_info_t* opcode_info = opcode_to_opcode_info(*bytes);
	    fprintf(stderr, "%s\n", opcode_info->string);
    }

    return 0;
}
