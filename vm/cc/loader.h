#ifndef __LOADER_H__
#define __LOADER_H__

#include <stdint.h>
#include <stddef.h>

#define NUMBER_OF_REGISTERS 64
#define MAX_OPCODE_STRING_SIZE 8
#define MAX_OPERAND_TYPES 8

#define GET_OPERAND(T)              \
    ({                              \
        T result = *(T*)(operands); \
        operands += sizeof(T);      \
        size += sizeof(T);          \
        result;                     \
    })

#define GET_VALUE(T, bytes) (*(T*)(bytes))
#define SET_VALUE(T, value, bytes) (*(T*)(bytes) = (value))

typedef int64_t  stack_value_type_t;
typedef uint8_t  register_type_t;
typedef uint32_t label_type_t;
typedef uint32_t address_type_t;
typedef int64_t  immediate_value_type_t;
typedef uint32_t stack_offset_type_;
typedef uint16_t data_length_type_t;
typedef uint8_t  arity_type_t;
typedef uint8_t  return_mode_type_t;
typedef uint16_t system_call_type_t;

typedef enum {
    // Register machine opcodes
    OPCODE_JMPRNZE,
    OPCODE_JMPRINGT,
    OPCODE_SUBRRI,
    OPCODE_SUBRSI,
    OPCODE_ADDRRI,
    OPCODE_LOADRI,
    OPCODE_PUSHR,
    OPCODE_LOADRS,
    OPCODE_LOADRR,
    OPCODE_RCALL,
    OPCODE_RRET,
    OPCODE_JMP,
    // Stack machine opcodes
    OPCODE_PUSH,
    OPCODE_PUSHS,
    OPCODE_POP,
    OPCODE_DUP,
    OPCODE_SWAP,
    OPCODE_LOAD,
    OPCODE_STORE,
    OPCODE_ADD,
    OPCODE_SUB,
    OPCODE_MUL,
    OPCODE_DIV,
    OPCODE_JUMP,
    OPCODE_CJUMP,
    OPCODE_CALL,
    OPCODE_RET,
    OPCODE_SYS,
    OPCODE_AND,
    OPCODE_OR,
    OPCODE_NOT,
    OPCODE_EQ,
    OPCODE_NEQ,
    OPCODE_LT,
    OPCODE_GT,
    OPCODE_NOP,
    OPCODE_HALT,
    OPCODE_MCALL,
    OPCODE_SPAWN,
    OPCODE_MSPAWN,
    OPCODE_ENUM_SIZE
} opcode_t;

typedef enum {
    OPERAND_STACK_VALUE,
    OPERAND_REGISTER,
    OPERAND_LABEL,
    OPERAND_IMMEDIATE_VALUE,
    OPERAND_STACK_OFFSET,
    OPERAND_ARITY,
    OPERAND_RETURN_MODE,
    OPERAND_SYSTEM_CALL,
    OPERAND_STRING
} operand_t;

typedef enum {
    SYSTEM_CALL_SELF,
    SYSTEM_CALL_SEND,
    SYSTEM_CALL_RECV,
    SYSTEM_CALL_PRINTLN,
    SYSTEM_CALL_DISPLAY,
    SYSTEM_CALL_EXIT,
    SYSTEM_CALL_INVALID,
    SYSTEM_CALL_ENUM_SIZE
} system_call_t;

typedef enum {
    RETURN_MODE_VALUE,
    RETURN_MODE_COPY,
    RETURN_MODE_INVALID,
    RETURN_MODE_ENUM_SIZE
} return_mode_t;

typedef struct {
    char string[MAX_OPCODE_STRING_SIZE];
    operand_t operands[MAX_OPERAND_TYPES];
} opcode_info_t;

const opcode_info_t* opcode_to_opcode_info(opcode_t opcode);
const opcode_info_t* string_to_opcode_info(const char* string);
const char* system_call_to_string(system_call_t system_call);
system_call_t string_to_system_call(const char* string);
const char* return_mode_to_string(return_mode_t return_mode);
return_mode_t string_to_return_mode(const char* string);

#endif
