#include "vm.h"
#include "log.h"

const opcode_info_t opcode_info_map[OPCODE_ENUM_SIZE] = {
    [OPCODE_JMPRNZE] = {
        .string = "jmprnze",
        .operands = {OPERAND_REGISTER, OPERAND_LABEL}
    }
};

const char* system_call_map[SYSTEM_CALL_ENUM_SIZE] = {
    [SYSTEM_CALL_SELF] = "self",
    [SYSTEM_CALL_SEND] = "send",
    [SYSTEM_CALL_RECV] = "recv",
    [SYSTEM_CALL_PRINTLN] = "println",
    [SYSTEM_CALL_DISPLAY] = "display",
    [SYSTEM_CALL_EXIT] = "exit"
};

const opcode_info_t* opcode_to_opcode_info(opcode_t opcode) {
    VM_ASSERT(opcode >= 0 && opcode <= OPCODE_ENUM_SIZE, "Invalid opcode");
    return &opcode_info_map[opcode];
}

const opcode_info_t* string_to_opcode_info(const char* string) {
    for (size_t i = 0; i < OPCODE_ENUM_SIZE; i++) {
        if (opcode_info_map[i].string == string) {
            return &opcode_info_map[i];
        }
    }
    VM_ABORT("Invalid opcode string");
    return NULL;
}
