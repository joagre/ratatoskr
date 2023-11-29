#ifndef __INSTRUCTIONS_H__
#define __INSTRUCTIONS_H__

#include "loader.h"

const opcode_info_t opcode_info_map[OPCODE_ENUM_SIZE] = {
    //
    // Register machine opcodes
    //
    [OPCODE_JMPRNZE] = {
        .opcode = OPCODE_JMPRNZE,
        .string = "jmprnze",
        .operands = {OPERAND_REGISTER, OPERAND_LABEL}
    },
    [OPCODE_JMPRINGT] = {
        .opcode = OPCODE_JMPRINGT,
        .string = "jmpringt",
        .operands = {OPERAND_REGISTER,
                     OPERAND_IMMEDIATE_VALUE,
                     OPERAND_LABEL}
    },
    [OPCODE_SUBRRI] = {
        .opcode = OPCODE_SUBRRI,
        .string = "subrri",
        .operands = {OPERAND_REGISTER,
                     OPERAND_REGISTER,
                     OPERAND_IMMEDIATE_VALUE}
    },
    [OPCODE_SUBRSI] = {
        .opcode = OPCODE_SUBRSI,
        .string = "subrsi",
        .operands = {OPERAND_REGISTER,
                     OPERAND_STACK_OFFSET,
                     OPERAND_IMMEDIATE_VALUE}
    },
    [OPCODE_ADDRRI] = {
        .opcode = OPCODE_JMPRINGT,
        .string = "addrri",
        .operands = {OPERAND_REGISTER,
                     OPERAND_REGISTER,
                     OPERAND_IMMEDIATE_VALUE}
    },
    [OPCODE_LOADRI] = {
        .opcode = OPCODE_LOADRI,
        .string = "loadri",
        .operands = {OPERAND_REGISTER,
                     OPERAND_IMMEDIATE_VALUE}
    },
    [OPCODE_PUSHR] = {
        .opcode = OPCODE_PUSHR,
        .string = "pushr",
        .operands = {OPERAND_REGISTER}
    },
    [OPCODE_LOADRS] = {
        .opcode = OPCODE_LOADRS,
        .string = "loadrs",
        .operands = {OPERAND_REGISTER,
                     OPERAND_STACK_OFFSET}
    },
    [OPCODE_LOADRR] = {
        .opcode = OPCODE_LOADRR,
        .string = "loadrr",
        .operands = {OPERAND_REGISTER,
                     OPERAND_REGISTER}
    },
    [OPCODE_RCALL] = {
        .opcode = OPCODE_RCALL,
        .string = "rcall",
        .operands = {OPERAND_LABEL}
    },
    [OPCODE_RRET] = {
        .opcode = OPCODE_RRET,
        .string = "rret",
        .operands = {}
    },
    [OPCODE_JMP] = {
        .opcode = OPCODE_JMP,
        .string = "jmp",
        .operands = {OPERAND_LABEL}
    },
    //
    // Stack machine opcodes
    //
    [OPCODE_PUSH] = {
        .opcode = OPCODE_PUSH,
        .string = "push",
        .operands = {OPERAND_STACK_VALUE}
    },
    [OPCODE_PUSHS] = {
        .opcode = OPCODE_PUSHS,
        .string = "pushs",
        .operands = {OPERAND_STRING}
    },
    [OPCODE_POP] = {
        .opcode = OPCODE_POP,
        .string = "pop",
        .operands = {}
    },
    [OPCODE_DUP] = {
        .opcode = OPCODE_DUP,
        .string = "dup",
        .operands = {}
    },
    [OPCODE_SWAP] = {
        .opcode = OPCODE_SWAP,
        .string = "swap",
        .operands = {}
    },
    [OPCODE_LOAD] = {
        .opcode = OPCODE_LOAD,
        .string = "load",
        .operands = {}
    },
    [OPCODE_STORE] = {
        .opcode = OPCODE_STORE,
        .string = "store",
        .operands = {}
    },
    [OPCODE_ADD] = {
        .opcode = OPCODE_ADD,
        .string = "add",
        .operands = {}
    },
    [OPCODE_SUB] = {
        .opcode = OPCODE_SUB,
        .string = "sub",
        .operands = {}
    },
    [OPCODE_MUL] = {
        .opcode = OPCODE_MUL,
        .string = "mul",
        .operands = {}
    },
    [OPCODE_DIV] = {
        .opcode = OPCODE_DIV,
        .string = "div",
        .operands = {}
    },
    [OPCODE_JUMP] = {
        .opcode = OPCODE_JUMP,
        .string = "jump",
        .operands = {OPERAND_LABEL}
    },
    [OPCODE_CJUMP] = {
        .opcode = OPCODE_CJUMP,
        .string = "cjump",
        .operands = {OPERAND_LABEL}
    },
    [OPCODE_CALL] = {
        .opcode = OPCODE_CALL,
        .string = "call",
        .operands = {OPERAND_LABEL, OPERAND_ARITY}
    },
    [OPCODE_RET] = {
        .opcode = OPCODE_RET,
        .string = "ret",
        .operands = {OPERAND_RETURN_MODE}
    },
    [OPCODE_SYS] = {
        .opcode = OPCODE_SYS,
        .string = "sys",
        .operands = {OPERAND_SYSTEM_CALL}
    },
    [OPCODE_AND] = {
        .opcode = OPCODE_AND,
        .string = "and",
        .operands = {}
    },
    [OPCODE_OR] = {
        .opcode = OPCODE_OR,
        .string = "or",
        .operands = {}
    },
    [OPCODE_NOT] = {
        .opcode = OPCODE_NOT,
        .string = "not",
        .operands = {}
    },
    [OPCODE_EQ] = {
        .opcode = OPCODE_EQ,
        .string = "eq",
        .operands = {}
    },
    [OPCODE_NEQ] = {
        .opcode = OPCODE_NEQ,
        .string = "neq",
        .operands = {}
    },
    [OPCODE_LT] = {
        .opcode = OPCODE_LT,
        .string = "lt",
        .operands = {}
    },
    [OPCODE_GT] = {
        .opcode = OPCODE_GT,
        .string = "gt",
        .operands = {}
    },
    [OPCODE_NOP] = {
        .opcode = OPCODE_NOP,
        .string = "nop",
        .operands = {}
    },
    [OPCODE_HALT] = {
        .opcode = OPCODE_HALT,
        .string = "halt",
        .operands = {}
    },
    [OPCODE_MCALL] = {
        .opcode = OPCODE_MCALL,
        .string = "mcall",
        .operands = {}
    },
    [OPCODE_SPAWN] = {
        .opcode = OPCODE_SPAWN,
        .string = "spawn",
        .operands = {OPERAND_LABEL, OPERAND_ARITY}
    },
    [OPCODE_MSPAWN] = {
        .opcode = OPCODE_MSPAWN,
        .string = "mspawn",
        .operands = {}
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
