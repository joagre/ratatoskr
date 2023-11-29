#include <string.h>
#include "vm.h"
#include "instructions.h"
#include "log.h"

const opcode_info_t* opcode_to_opcode_info(opcode_t opcode) {
    SATIE_ASSERT(opcode >= 0 && opcode <= OPCODE_ENUM_SIZE, "Invalid opcode");
    return &opcode_info_map[opcode];
}

const opcode_info_t* string_to_opcode_info(const char* string) {
    for (size_t i = 0; i < OPCODE_ENUM_SIZE; i++) {
        if (strcmp(opcode_info_map[i].string, string) == 0) {
            return &opcode_info_map[i];
        }
    }
    SATIE_ABORT("Invalid opcode string");
    return NULL;
}

const char* system_call_to_string(system_call_t system_call) {
    return system_call_map[system_call];
}

system_call_t string_to_system_call(const char* string) {
    for (size_t i = 0; i < SYSTEM_CALL_ENUM_SIZE; i++) {
        if (strcmp(system_call_map[i], string) == 0) {
            return i;
        }
    }
    SATIE_ABORT("Invalid system call string");
    return SYSTEM_CALL_INVALID;
}

const char* return_mode_to_string(return_mode_t return_mode) {
    return return_mode_map[return_mode];
}

return_mode_t string_to_return_mode(const char* string) {
    for (size_t i = 0; i < RETURN_MODE_ENUM_SIZE; i++) {
        if (strcmp(return_mode_map[i], string) == 0) {
            return i;
        }
    }
    SATIE_ABORT("Invalid return mode string");
    return RETURN_MODE_INVALID;
}
