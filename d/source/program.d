module program;

import std.stdio : File, writeln;
import std.string : strip, split, indexOf;
import std.conv : to, ConvException;
import std.typecons : Tuple;
import std.algorithm.searching : canFind;
import std.utf: toUTF8;
import std.regex: replace, regex;

struct Registers {
    static const ubyte SP = 0;
    static const ubyte FP = 1;
}

struct Opcodes {
    static const ubyte PUSH  = 0;
    static const ubyte PUSHS = 1;
    static const ubyte POP   = 2;
    static const ubyte DUP   = 3;
    static const ubyte SWAP  = 4;
    static const ubyte LOAD  = 5;
    static const ubyte STORE = 6;
    static const ubyte ADD   = 7;
    static const ubyte SUB   = 8;
    static const ubyte MUL   = 9;
    static const ubyte DIV   = 10;
    static const ubyte JUMP  = 11;
    static const ubyte CJUMP = 12;
    static const ubyte CALL  = 13;
    static const ubyte RET   = 14;
    static const ubyte SYS   = 15;
    static const ubyte AND   = 16;
    static const ubyte OR    = 17;
    static const ubyte NOT   = 18;
    static const ubyte EQ    = 19;
    static const ubyte NEQ   = 20;
    static const ubyte LT    = 21;
    static const ubyte GT    = 22;
    static const ubyte NOP   = 23;
    static const ubyte HALT  = 24;
}

struct SystemCalls {
    static const long SPAWN   = 0;
    static const long SEND    = 1;
    static const long RECV    = 2;
    static const long PRINTLN = 3;
}

struct ReturnModes {
    static const ubyte VALUE = 0;
    static const ubyte COPY = 1;
}

class ByteCodeError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
}

struct Program {
    public ubyte[] byte_code;
    public int[int] jump_table;
    public string filename;
    private File file;

    this(string filename) {
        this.filename = filename;
        file = File(filename, "r");
        try {
            generateByteCode();
        } finally {
            file.close;
        }
    }

    private void generateByteCode() {
        while (!file.eof) {
            string line = file.readln.strip;

            if (line.length == 0) {
                continue;
            } else {
                // Remove comments
                line = line.split(";")[0].strip;
                // Remove duplicated whitespaces with single blanks
                line = line.replace(regex(r"\s+"), " ");
                if (line.length == 0) {
                    continue;
                }
            }

            // Extract opcode and operands
            auto first_blank = line.indexOf(" ");
            string opcode;
            string operands = null;
            if (first_blank == -1) {
                opcode = line.strip;
            } else {
                opcode = line[0 .. first_blank];
                operands = line[first_blank + 1 .. $];
            }

            switch (opcode) {
            case "LABEL":
                auto parts = operands.split;
                assert_operands(parts.length, 1, line);
                jump_table[parse!int(parts[0], line)] =
                    cast(int)byte_code.length;
                continue;
            case "PUSH":
                auto parts = operands.split;
                assert_operands(parts.length, 1, line);
                byte_code ~= Opcodes.PUSH << 3;
                insert(parse!long(parts[0], line), byte_code);
                break;
            case "PUSHS":
                if (operands.length == 0) {
                    throw new ByteCodeError("Invalid instruction " ~ line);
                }
                byte_code ~= Opcodes.PUSHS << 3;
                ubyte[] bytes = cast(ubyte[])toUTF8(operands.strip(`"`));
                insert(cast(int)bytes.length, byte_code);
                byte_code ~= bytes;
                break;
            case "POP":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.POP << 3;
                break;
            case "DUP":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.DUP << 3;
                break;
            case "SWAP":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.SWAP << 3;
                break;
            case "LOAD":
                auto parts = operands.split;
                assert_operands(parts.length, 1, line);
                byte_code ~= add_register(parts[0], Opcodes.LOAD, line);
                break;
            case "STORE":
                auto parts = operands.split;
                assert_operands(parts.length, 1, line);
                byte_code ~= add_register(parts[0], Opcodes.STORE, line);
                break;
            case "ADD":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.ADD << 3;
                break;
            case "SUB":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.SUB << 3;
                break;
            case "MUL":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.MUL << 3;
                break;
            case "DIV":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.DIV << 3;
                break;
            case "JUMP":
                auto parts = operands.split;
                assert_operands(parts.length, 1, line);
                byte_code ~= Opcodes.JUMP << 3;
                insert(parse!long(parts[0], line), byte_code);
                break;
            case "CJUMP":
                auto parts = operands.split;
                assert_operands(parts.length, 1, line);
                byte_code ~= Opcodes.CJUMP << 3;
                insert(parse!long(parts[0], line), byte_code);
                break;
            case "CALL":
                auto parts = operands.split;
                assert_operands(parts.length, 2, line);
                byte_code ~= Opcodes.CALL << 3;
                insert(parse!int(parts[0], line), byte_code);
                insert(parse!int(parts[1], line), byte_code);
                break;
            case "RET":
                auto parts = operands.split;
                if (parts.length == 0) {
                    byte_code ~= (Opcodes.RET << 3) | ReturnModes.VALUE;
                } else if (parts.length == 1 && parts[0] == "copy") {
                    byte_code ~= (Opcodes.RET << 3) | ReturnModes.COPY;
                } else {
                    throw new ByteCodeError("Invalid instruction " ~ line);
                }
                break;
            case "SYS":
                auto parts = operands.split;
                assert_operands(parts.length, 1, line);
                byte_code ~= Opcodes.SYS << 3;
                long sys_name;
                switch(parts[0]) {
                case "spawn":
                    sys_name = SystemCalls.SPAWN;
                    break;
                case "send":
                    sys_name = SystemCalls.SEND;
                    break;
                case "recv":
                    sys_name = SystemCalls.RECV;
                    break;
                case "println":
                    sys_name = SystemCalls.PRINTLN;
                    break;
                default:
                    throw new ByteCodeError("Invalid instruction " ~ line);
                }
                insert(sys_name, byte_code);
                break;
            case "AND":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.AND << 3;
                break;
            case "OR":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.OR << 3;
                break;
            case "NOT":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.NOT << 3;
                break;
            case "EQ":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.EQ << 3;
                break;
            case "NEQ":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.NEQ << 3;
                break;
            case "LT":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.LT << 3;
                break;
            case "GT":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.GT << 3;
                break;
            case "NOP":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.NOP << 3;
                break;
            case "HALT":
                assert_no_operands(operands, line);
                byte_code ~= Opcodes.HALT << 3;
                break;
            default:
                throw new ByteCodeError("Invalid instruction " ~ line);
            }
        }

        // Convert labels to byte indices
        long i = 0;
        while (i < byte_code.length) {
            auto opcode = byte_code[i] >> 3;
            if (opcode == Opcodes.PUSH) {
                i += 8;
            } else if (opcode == Opcodes.JUMP || opcode == Opcodes.CJUMP ||
                       opcode == Opcodes.CALL) {
                auto label = get!int(&byte_code[i + 1]);
                auto byte_index = jump_table[label];
                set!int(byte_index, &byte_code[i + 1]);
                i += 8;
            } else if (opcode == Opcodes.SYS) {
                i += 8;
            }
            i++;
        }
    }

    private void assert_operands(ulong arity, int expected_arity, string line) {
        if (arity != expected_arity) {
            throw new ByteCodeError("Invalid instruction " ~ line);
        }
    }

    private T parse(T)(string value, string line)
         if (is(T == int) || is(T == long)) {
             try {
                 return to!T(value);
             } catch (ConvException) {
                 throw new ByteCodeError("Invalid instruction " ~ line);
             }
         }

    private void assert_no_operands(string operands, string line) {
        if (operands.length != 0) {
            throw new ByteCodeError("Invalid instruction " ~ line);
        }
    }

    private ubyte add_register(string register, ubyte opcode, string line) {
        if (register == "SP") {
            return cast(ubyte)(opcode << 3) | Registers.SP;
        } else if (register == "FP") {
            return cast(ubyte)(opcode << 3) | Registers.FP;
        } else {
            throw new ByteCodeError("Invalid instruction " ~ line);
        }
    }

    public void pretty_print() {
        long i = 0;
        while (i < byte_code.length) {
            i += 1 + pretty_print(&byte_code[i], true);
        }
    }

    public long pretty_print(ubyte* bytes, bool show_labels) {
        switch (bytes[0] >> 3) {
        case Opcodes.PUSH:
            long value = get!long(&bytes[1]);
            writeln("PUSH " ~ to!string(value));
            return 8;
        case Opcodes.PUSHS:
            auto length = get!int(&bytes[1]);
            auto index = 1 + 4;
            auto byte_string = bytes[index .. index + length + 1];
            writeln("PUSHS \"" ~ cast(string)byte_string ~ "\"");
            return 4 + length;
        case Opcodes.POP:
            writeln("POP");
            break;
        case Opcodes.DUP:
            writeln("DUP");
            break;
        case Opcodes.SWAP:
            writeln("SWAP");
            break;
        case Opcodes.LOAD:
            string register = get_register_string(bytes[0]);
            writeln("LOAD " ~ register);
            break;
        case Opcodes.STORE:
            string register = get_register_string(bytes[0]);
            writeln("STORE " ~ register);
            break;
        case Opcodes.ADD:
            writeln("ADD");
            break;
        case Opcodes.SUB:
            writeln("SUB");
            break;
        case Opcodes.MUL:
            writeln("MUL");
            break;
        case Opcodes.DIV:
            writeln("DIV");
            break;
        case Opcodes.JUMP:
            auto byte_index = get!long(&bytes[1]);
            if (show_labels) {
                auto label = lookup_label(byte_index);
                writeln("JUMP " ~ to!string(label));
            } else {
                writeln("JUMP " ~ to!string(byte_index));
            }
            return 8;
        case Opcodes.CJUMP:
            auto byte_index = get!long(&bytes[1]);
            if (show_labels) {
                auto label = lookup_label(byte_index);
                writeln("CJUMP " ~ to!string(label));
            } else {
                writeln("CJUMP " ~ to!string(byte_index));
            }
            return 8;
        case Opcodes.CALL:
            auto byte_index = get!int(&bytes[1]);
            auto arity = get!int(&bytes[5]);
            if (show_labels) {
                auto label = lookup_label(byte_index);
                writeln("CALL " ~ to!string(label) ~ " " ~ to!string(arity));
            } else {
                writeln("CALL " ~ to!string(byte_index) ~ " " ~
                        to!string(arity));
            }
            return 8;
        case Opcodes.RET:
            auto return_mode = bytes[0] & 0b00000111;
            if (return_mode == ReturnModes.COPY) {
                writeln("RET copy");
            } else {
                writeln("RET");
            }
            break;
        case Opcodes.SYS:
            long value = get!long(&bytes[1]);
            writeln("SYS " ~ to!string(value));
            return 8;
        case Opcodes.AND:
            writeln("AND");
            break;
        case Opcodes.OR:
            writeln("OR");
            break;
        case Opcodes.NOT:
            writeln("NOT");
            break;
        case Opcodes.EQ:
            writeln("EQ");
            break;
        case Opcodes.NEQ:
            writeln("NEQ");
            break;
        case Opcodes.LT:
            writeln("LT");
            break;
        case Opcodes.GT:
            writeln("GT");
            break;
        case Opcodes.NOP:
            writeln("NOP");
            break;
        case Opcodes.HALT:
            writeln("HALT");
            break;
        default:
            throw new ByteCodeError("Unknown opcode " ~
                                    to!string(bytes[0] >> 3));
        }

        return 0;
    }

    private long lookup_label(long byte_index) {
        foreach (label, possible_byte_index; jump_table) {
            if (byte_index == possible_byte_index) {
                return label;
            }
        }
        throw new ByteCodeError("Internal Error");
    }

    private string get_register_string(ubyte instruction) {
        auto register = instruction & 0b00000111;
        if (register == Registers.SP) {
            return "SP";
        } else { // Must be FP
            return "FP";
        }
    }
}

public void insert(T)(T value, ref ubyte[] bytes) {
    bytes ~= (cast(ubyte*)&value)[0 .. T.sizeof];
}

public T get(T)(ubyte* bytes) {
    return *cast(T*)bytes;
}

public void set(T)(T value, ubyte* bytes) {
    *cast(T*)bytes = value;
}
