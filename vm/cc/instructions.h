#ifndef __INSTRUCTIONS_H__
#define __INSTRUCTIONS_H__

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

const char* return_mode_map[RETURN_MODE_ENUM_SIZE] = {
    [RETURN_MODE_VALUE] = "value",
    [RETURN_MODE_COPY] = "copy"
};

#endif
