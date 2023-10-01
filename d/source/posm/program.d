module program;

import std.stdio;
import std.string;
import std.conv;
import std.regex;
import std.traits;
import std.algorithm.searching;

enum Opcode {
    PUSH,
    PUSHR,
    POP,
    DUP,
    SWAP,
    LOADR,
    STORER,
    MOVER,
    ADD,
    SUB,
    MUL,
    DIV,
    JUMP,
    CJUMP,
    CALL,
    RET,
    SYS,
    AND,
    OR,
    NOT,
    EQ,
    NEQ,
    LT,
    GT,
    NOP,
    HALT
}

const SP = 0;
const FP = 1;
const PC = 2;

const SYS_SPAWN = 0;
const SYS_SEND = 1;
const SYS_RECV = 2;
const SYS_RAND = 3;
const SYS_SLEEP = 4;
const SYS_PRINTLN = 5;

struct Instruction {
    Opcode opcode;
    ulong operand = 0;

    this(const Opcode opcode, const ulong operand) {
        this.opcode = opcode;
        this.operand = operand;
    }

    this(const Opcode opcode) {
        this.opcode = opcode;
    }
}

class InstructionError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
}

struct Program {
    public Instruction[] instructions;
    public ulong[ulong] jumpTable;
    private File file;

    this(const string filename) {
        file = File(filename, "r");
        try {
            loadInstructions();
        } finally {
            file.close;
        }
    }

    private void loadInstructions() {
        while (!file.eof) {
            auto line = file.readln.strip;

            if (line.length == 0) {
                continue;
            }

            // Remove comments
            line = line.split(";")[0];
            // Remove duplicated whitespace and return with a single blankspace
            line = replace(line, regex(`\s+`), " ");

            if (line.length == 0) {
                continue;
            }

            immutable auto parts = line.strip.split;

            Instruction instruction;

            final switch (parts[0]) {
            case "LABEL":
                jumpTable[to_value(parts[1])] = instructions.length;
                continue;
            case "PUSH":
                instruction = Instruction(Opcode.PUSH, to_value(parts[1]));
                break;
            case "PUSHR":
                instruction = Instruction(Opcode.PUSHR, to_register(parts[1]));
                break;
            case "POP":
                instruction = Instruction(Opcode.POP);
                break;
            case "DUP":
                instruction = Instruction(Opcode.DUP);
                break;
            case "SWAP":
                instruction = Instruction(Opcode.SWAP);
                break;
            case "LOADR":
                instruction =
                    Instruction(Opcode.LOADR, to_register(parts[1], [SP, FP]));
                break;
            case "STORER":
                instruction =
                    Instruction(Opcode.STORER, to_register(parts[1], [SP, FP]));
                break;
            case "MOVER":
                instruction = Instruction(Opcode.MOVER, to_register(parts[1]));
                break;
            case "ADD":
                instruction = Instruction(Opcode.ADD);
                break;
            case "SUB":
                instruction = Instruction(Opcode.SUB);
                break;
            case "MUL":
                instruction = Instruction(Opcode.MUL);
                break;
            case "DIV":
                instruction = Instruction(Opcode.DIV);
                break;
            case "JUMP":
                instruction = Instruction(Opcode.JUMP, to_value(parts[1]));
                break;
            case "CJUMP":
                instruction = Instruction(Opcode.CJUMP, to_value(parts[1]));
                break;
            case "CALL":
                instruction = Instruction(Opcode.CALL, to_value(parts[1]));
                break;
            case "RET":
                instruction = Instruction(Opcode.RET);
                break;
            case "SYS":
                instruction = Instruction(Opcode.RET, to_sys_name(parts[1]));
                break;
            case "AND":
                instruction = Instruction(Opcode.AND);
                break;
            case "OR":
                instruction = Instruction(Opcode.OR);
                break;
            case "NOT":
                instruction = Instruction(Opcode.NOT);
                break;
            case "EQ":
                instruction = Instruction(Opcode.EQ);
                break;
            case "NEQ":
                instruction = Instruction(Opcode.NEQ);
                break;
            case "LT":
                instruction = Instruction(Opcode.LT);
                break;
            case "GT":
                instruction = Instruction(Opcode.GT);
                break;
            case "NOP":
                instruction = Instruction(Opcode.NOP);
                break;
            case "HALT":
                instruction = Instruction(Opcode.HALT);
                break;
            }

            instructions ~= instruction;
        }

        foreach (index, value; instructions) {
            if (value.opcode == Opcode.JUMP ||
                value.opcode == Opcode.CJUMP ||
                value.opcode == Opcode.CALL ||
                value.opcode == Opcode.SYS) {
                instructions[index].operand = jumpTable[value.operand];
            }
        }
    }

    private ulong to_value(const string s) {
        try {
            return to!ulong(s);
        } catch (ConvException e) {
            throw new InstructionError("Bad value " ~ s);
        }
    }

    private ulong to_register(const string s, const uint[] valid_registers) {
        auto register = to_register(s);
        if (valid_registers.canFind(register)) {
            return register;
        } else {
            throw new InstructionError("Invalid register " ~ s);
        }
    }

    private ulong to_register(const string s) {
        switch (s) {
        case "SP":
            return SP;
        case "FP":
            return FP;
        case "PC":
            return PC;
        default:
            throw new InstructionError("Unknown register " ~ s);
        }
    }

    private ulong to_sys_name(const string s) {
        switch (s) {
        case "spawn":
            return SYS_SPAWN;
        case "send":
            return SYS_SEND;
        case "recv":
            return SYS_RECV;
        case "rand":
            return SYS_RAND;
        case "sleep":
            return SYS_SLEEP;
        case "println":
            return SYS_PRINTLN;
        default:
            throw new InstructionError("Unknown system function " ~ s);
        }
    }

    public void pretty_print() {
        foreach (index, instruction; instructions) {
            pretty_print(index, instruction);
        }
    }

    public void pretty_print(size_t index, Instruction instruction) {
        string opcode = enumToName(instruction.opcode);
        if (instruction.opcode == Opcode.PUSH) {
            writeln(to!string(index) ~ ": " ~ opcode ~ " " ~
                    to!string(instruction.operand));
        } else if (instruction.opcode == Opcode.PUSHR ||
                   instruction.opcode == Opcode.LOADR ||
                   instruction.opcode == Opcode.STORER ||
                   instruction.opcode == Opcode.MOVER) {
            auto register = from_register(instruction.operand);
            writeln(to!string(index) ~ ": " ~ opcode ~ " " ~ register);
        } else if (instruction.opcode == Opcode.PUSH ||
                   instruction.opcode == Opcode.JUMP ||
                   instruction.opcode == Opcode.CJUMP ||
                   instruction.opcode == Opcode.CALL) {
            writeln(to!string(index) ~ ": " ~ opcode ~ " " ~
                    to!string(instruction.operand));
        } else if (instruction.opcode == Opcode.SYS) {
            auto name = from_sys_name(instruction.operand);
            writeln(to!string(index) ~ ": " ~ opcode ~ " " ~ name);
        } else {
            writeln(to!string(index) ~ ": " ~ opcode);
        }
    }

    private string enumToName(T)(T value) {
        return to!string([EnumMembers!T][value]);
    }

    private string from_register(const ulong register) {
        final switch (register) {
        case SP:
            return "SP";
        case FP:
            return "FP";
        case PC:
            return "PC";
        }
    }

    private string from_sys_name(const ulong name) {
        final switch (name) {
        case SYS_SPAWN:
            return "spawn";
        case SYS_SEND:
            return "send";
        case SYS_RECV:
            return "recv";
        case SYS_RAND:
            return "rand";
        case SYS_SLEEP:
            return "sleep";
        case SYS_PRINTLN:
            return "println";
        }
    }
}
