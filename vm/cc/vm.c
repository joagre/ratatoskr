#include <string.h>
#include "instructions.h"

opcode_info_t* opcode_to_opcode_info(opcode_t opcode) {
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

char* return_mode_to_string(return_mode_t return_mode) {
    return return_mode_map[return_mode];
}

return_mode_t string_to_return_mode(char* string, satie_error_t* error) {
    for (size_t i = 0; i < RETURN_MODE_ENUM_SIZE; i++) {
        if (strcmp(return_mode_map[i], string) == 0) {
            CLEAR_ERROR(error);
            return i;
        }
    }
    SET_ERROR_MESSAGE(error, COMPONENT_VM, "Invalid return mode: %s", string);
    return RETURN_MODE_INVALID;
}
