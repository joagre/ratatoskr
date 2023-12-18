#ifndef __VM_H__
#define __VM_H__

#include <stdint.h>
#include "satie_error.h"

#define NUMBER_OF_REGISTERS 64
#define MAX_OPCODE_STRING_SIZE 16
#define MAX_OPERANDS_STRING_SIZE 128
#define MAX_OPERANDS 8
#define MAX_OPERAND_STRING_SIZE 32
#define MAX_LINE_LENGTH 256
#define INITIAL_BYTE_CODE_SIZE 512
#define OPCODE_SIZE sizeof(uint8_t)
#define INITIAL_CALL_STACK_SIZE (128*1024)

#define GET_OPERAND(T)                          \
    ({ \
        T result = *(T*)(operands); \
        operands += sizeof(T); \
        size += sizeof(T); \
        result; \
    })
#define GET_VALUE(T, bytes) (*(T*)(bytes))
#define SET_VALUE(T, value, bytes) (*(T*)(bytes) = (value))

typedef int64_t  vm_stack_value_t;
typedef uint8_t  vm_register_t;
typedef uint32_t vm_label_t;
typedef uint32_t vm_address_t;
typedef int64_t  vm_immediate_value_t;
typedef uint32_t vm_stack_offset_t;
typedef uint16_t vm_data_length_t;
typedef uint8_t  vm_arity_t;
typedef uint16_t vm_system_call_t;

typedef enum {
    OPCODE_JMPRINEQ = 0,
    OPCODE_JMPRNZE,
    OPCODE_JMPRINGT,
    OPCODE_SUBRRI,
    OPCODE_SUBRSI,
    OPCODE_ADDRRI,
    OPCODE_LOADRI,
    OPCODE_PUSHI,
    OPCODE_PUSHR,
    OPCODE_POP,
    OPCODE_LOADRS,
    OPCODE_LOADRR,
    OPCODE_CALL,
    OPCODE_RET,
    OPCODE_JMP,
    OPCODE_SYS,
    OPCODE_MCALL, // FIXME
    OPCODE_SPAWN, // FIXME
    OPCODE_MSPAWN, // FIXME
    OPCODE_ENUM_SIZE
} opcode_t;

typedef enum {
    OPERAND_STACK_VALUE = 0,
    OPERAND_REGISTER,
    OPERAND_LABEL,
    OPERAND_IMMEDIATE_VALUE,
    OPERAND_STACK_OFFSET,
    OPERAND_ARITY,
    OPERAND_SYSTEM_CALL,
    OPERAND_STRING
} operand_t;

typedef enum {
    SYSTEM_CALL_SELF = 0,
    SYSTEM_CALL_SEND,
    SYSTEM_CALL_RECV,
    SYSTEM_CALL_PRINTLN,
    SYSTEM_CALL_DISPLAY,
    SYSTEM_CALL_EXIT,
    SYSTEM_CALL_INVALID,
    SYSTEM_CALL_ENUM_SIZE
} system_call_t;

typedef struct {
    opcode_t opcode;
    char string[MAX_OPCODE_STRING_SIZE];
    operand_t operands[MAX_OPERANDS];
    size_t number_of_operands;
} opcode_info_t;

opcode_info_t* opcode_to_opcode_info(opcode_t opcode);
opcode_info_t* string_to_opcode_info(char* string, satie_error_t* error);
char* system_call_to_string(system_call_t system_call);
system_call_t string_to_system_call(char* string, satie_error_t* error);

#endif
