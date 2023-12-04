#ifndef __INSTRUCTIONS_H__
#define __INSTRUCTIONS_H__

#include "loader.h"

opcode_info_t opcode_info_map[OPCODE_ENUM_SIZE] = {
    //
    // Register machine opcodes
    //
    [OPCODE_JMPRNZE] = {
        .opcode = OPCODE_JMPRNZE,
        .string = "jmprnze",
        .operands = {OPERAND_REGISTER, OPERAND_LABEL},
        .number_of_operands = 2
    },
    [OPCODE_JMPRINGT] = {
        .opcode = OPCODE_JMPRINGT,
        .string = "jmpringt",
        .operands = {OPERAND_REGISTER,
                     OPERAND_IMMEDIATE_VALUE,
                     OPERAND_LABEL},
        .number_of_operands = 3
    },
    [OPCODE_SUBRRI] = {
        .opcode = OPCODE_SUBRRI,
        .string = "subrri",
        .operands = {OPERAND_REGISTER,
                     OPERAND_REGISTER,
                     OPERAND_IMMEDIATE_VALUE},
        .number_of_operands = 3
    },
    [OPCODE_SUBRSI] = {
        .opcode = OPCODE_SUBRSI,
        .string = "subrsi",
        .operands = {OPERAND_REGISTER,
                     OPERAND_STACK_OFFSET,
                     OPERAND_IMMEDIATE_VALUE},
        .number_of_operands = 3
    },
    [OPCODE_ADDRRI] = {
        .opcode = OPCODE_ADDRRI,
        .string = "addrri",
        .operands = {OPERAND_REGISTER,
                     OPERAND_REGISTER,
                     OPERAND_IMMEDIATE_VALUE},
        .number_of_operands = 3
    },
    [OPCODE_LOADRI] = {
        .opcode = OPCODE_LOADRI,
        .string = "loadri",
        .operands = {OPERAND_REGISTER,
                     OPERAND_IMMEDIATE_VALUE},
        .number_of_operands = 2
    },
    [OPCODE_PUSHR] = {
        .opcode = OPCODE_PUSHR,
        .string = "pushr",
        .operands = {OPERAND_REGISTER},
        .number_of_operands = 1
    },
    [OPCODE_LOADRS] = {
        .opcode = OPCODE_LOADRS,
        .string = "loadrs",
        .operands = {OPERAND_REGISTER,
                     OPERAND_STACK_OFFSET},
        .number_of_operands = 2
    },
    [OPCODE_LOADRR] = {
        .opcode = OPCODE_LOADRR,
        .string = "loadrr",
        .operands = {OPERAND_REGISTER,
                     OPERAND_REGISTER},
        .number_of_operands = 2
    },
    [OPCODE_RCALL] = {
        .opcode = OPCODE_RCALL,
        .string = "rcall",
        .operands = {OPERAND_LABEL},
        .number_of_operands = 1
    },
    [OPCODE_RRET] = {
        .opcode = OPCODE_RRET,
        .string = "rret",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_JMP] = {
        .opcode = OPCODE_JMP,
        .string = "jmp",
        .operands = {OPERAND_LABEL},
        .number_of_operands = 1
    },
    //
    // Stack machine opcodes
    //
    [OPCODE_PUSH] = {
        .opcode = OPCODE_PUSH,
        .string = "push",
        .operands = {OPERAND_STACK_VALUE},
        .number_of_operands = 1

    },
    [OPCODE_PUSHS] = {
        .opcode = OPCODE_PUSHS,
        .string = "pushs",
        .operands = {OPERAND_STRING},
        .number_of_operands = 1
    },
    [OPCODE_POP] = {
        .opcode = OPCODE_POP,
        .string = "pop",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_DUP] = {
        .opcode = OPCODE_DUP,
        .string = "dup",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_SWAP] = {
        .opcode = OPCODE_SWAP,
        .string = "swap",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_LOAD] = {
        .opcode = OPCODE_LOAD,
        .string = "load",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_STORE] = {
        .opcode = OPCODE_STORE,
        .string = "store",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_ADD] = {
        .opcode = OPCODE_ADD,
        .string = "add",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_SUB] = {
        .opcode = OPCODE_SUB,
        .string = "sub",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_MUL] = {
        .opcode = OPCODE_MUL,
        .string = "mul",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_DIV] = {
        .opcode = OPCODE_DIV,
        .string = "div",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_JUMP] = {
        .opcode = OPCODE_JUMP,
        .string = "jump",
        .operands = {OPERAND_LABEL},
        .number_of_operands = 1
    },
    [OPCODE_CJUMP] = {
        .opcode = OPCODE_CJUMP,
        .string = "cjump",
        .operands = {OPERAND_LABEL},
        .number_of_operands = 1
    },
    [OPCODE_CALL] = {
        .opcode = OPCODE_CALL,
        .string = "call",
        .operands = {OPERAND_LABEL, OPERAND_ARITY},
        .number_of_operands = 2
    },
    [OPCODE_RET] = {
        .opcode = OPCODE_RET,
        .string = "ret",
        .operands = {OPERAND_RETURN_MODE},
        .number_of_operands = 1
    },
    [OPCODE_SYS] = {
        .opcode = OPCODE_SYS,
        .string = "sys",
        .operands = {OPERAND_SYSTEM_CALL},
        .number_of_operands = 1
    },
    [OPCODE_AND] = {
        .opcode = OPCODE_AND,
        .string = "and",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_OR] = {
        .opcode = OPCODE_OR,
        .string = "or",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_NOT] = {
        .opcode = OPCODE_NOT,
        .string = "not",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_EQ] = {
        .opcode = OPCODE_EQ,
        .string = "eq",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_NEQ] = {
        .opcode = OPCODE_NEQ,
        .string = "neq",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_LT] = {
        .opcode = OPCODE_LT,
        .string = "lt",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_GT] = {
        .opcode = OPCODE_GT,
        .string = "gt",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_NOP] = {
        .opcode = OPCODE_NOP,
        .string = "nop",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_HALT] = {
        .opcode = OPCODE_HALT,
        .string = "halt",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_MCALL] = {
        .opcode = OPCODE_MCALL,
        .string = "mcall",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_SPAWN] = {
        .opcode = OPCODE_SPAWN,
        .string = "spawn",
        .operands = {OPERAND_LABEL, OPERAND_ARITY},
        .number_of_operands = 2
    },
    [OPCODE_MSPAWN] = {
        .opcode = OPCODE_MSPAWN,
        .string = "mspawn",
        .operands = {},
        .number_of_operands = 0
    }
};

char* system_call_map[SYSTEM_CALL_ENUM_SIZE] = {
    [SYSTEM_CALL_SELF] = "self",
    [SYSTEM_CALL_SEND] = "send",
    [SYSTEM_CALL_RECV] = "recv",
    [SYSTEM_CALL_PRINTLN] = "println",
    [SYSTEM_CALL_DISPLAY] = "display",
    [SYSTEM_CALL_EXIT] = "exit"
};

char* return_mode_map[RETURN_MODE_ENUM_SIZE] = {
    [RETURN_MODE_VALUE] = "value",
    [RETURN_MODE_COPY] = "copy"
};

#endif
