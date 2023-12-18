#ifndef __INSTRUCTIONS_H__
#define __INSTRUCTIONS_H__

#include "loader.h"

opcode_info_t opcode_info_map[OPCODE_ENUM_SIZE] = {
    [OPCODE_JMPRINEQ] = {
        .opcode = OPCODE_JMPRINEQ,
        .string = "jmprineq",
        .operands = {OPERAND_REGISTER,
                     OPERAND_IMMEDIATE_VALUE,
                     OPERAND_LABEL},
        .number_of_operands = 3
    },
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
    [OPCODE_PUSHI] = {
        .opcode = OPCODE_PUSHI,
        .string = "pushi",
        .operands = {OPERAND_IMMEDIATE_VALUE},
        .number_of_operands = 1
    },
    [OPCODE_PUSHR] = {
        .opcode = OPCODE_PUSHR,
        .string = "pushr",
        .operands = {OPERAND_REGISTER},
        .number_of_operands = 1
    },
    [OPCODE_POP] = {
        .opcode = OPCODE_POP,
        .string = "pop",
        .operands = {},
        .number_of_operands = 0
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
    [OPCODE_CALL] = {
        .opcode = OPCODE_CALL,
        .string = "call",
        .operands = {OPERAND_LABEL},
        .number_of_operands = 1
    },
    [OPCODE_RET] = {
        .opcode = OPCODE_RET,
        .string = "ret",
        .operands = {},
        .number_of_operands = 0
    },
    [OPCODE_JMP] = {
        .opcode = OPCODE_JMP,
        .string = "jmp",
        .operands = {OPERAND_LABEL},
        .number_of_operands = 1
    },
    [OPCODE_SYS] = {
        .opcode = OPCODE_SYS,
        .string = "sys",
        .operands = {OPERAND_SYSTEM_CALL},
        .number_of_operands = 1
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
    [SYSTEM_CALL_EXIT] = "exit",
    [SYSTEM_CALL_INVALID] = "invalid"
};

#endif
