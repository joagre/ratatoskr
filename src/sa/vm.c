#include <string.h>
#include "instructions.h"
#include "log.h"

opcode_info_t* opcode_to_opcode_info(opcode_t opcode) {
    LOG_ASSERT(opcode < OPCODE_ENUM_SIZE, "Invalid opcode: %d", opcode);
    return &opcode_info_map[opcode];
}

opcode_info_t* string_to_opcode_info(char* string, satie_error_t* error) {
    for (size_t i = 0; i < OPCODE_ENUM_SIZE; i++) {
        if (strcmp(opcode_info_map[i].string, string) == 0) {
            CLEAR_ERROR(error);
            return &opcode_info_map[i];
        }
    }
    SET_ERROR_MESSAGE(error, COMPONENT_VM, "Invalid opcode: %s", string);
    return NULL;
}

char* system_call_to_string(system_call_t system_call) {
    return system_call_map[system_call];
}

system_call_t string_to_system_call(char* string, satie_error_t* error) {
    for (size_t i = 0; i < SYSTEM_CALL_ENUM_SIZE; i++) {
        if (strcmp(system_call_map[i], string) == 0) {
            CLEAR_ERROR(error);
            return i;
        }
    }
    SET_ERROR_MESSAGE(error, COMPONENT_VM, "Invalid system call: %s", string);
    return SYSTEM_CALL_INVALID;
}
