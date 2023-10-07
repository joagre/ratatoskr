module program;

import std.stdio : File, writeln;
import std.string : strip, split, indexOf;
import std.conv : to, ConvException;
import std.regex : regex, replace;
import std.typecons : Tuple;
import std.algorithm.searching : canFind;
import std.utf: toUTF8;

const ubyte SP = 0;
const ubyte FP = 1;

const ubyte PUSH  = 0;
const ubyte PUSHS = 1;
const ubyte POP   = 2;
const ubyte DUP   = 3;
const ubyte SWAP  = 4;
const ubyte LOAD  = 5;
const ubyte STORE = 6;
const ubyte ADD   = 7;
const ubyte SUB   = 8;
const ubyte MUL   = 9;
const ubyte DIV   = 10;
const ubyte JUMP  = 11;
const ubyte CJUMP = 12;
const ubyte CALL  = 13;
const ubyte RET   = 14;
const ubyte SYS   = 15;
const ubyte AND   = 16;
const ubyte OR    = 17;
const ubyte NOT   = 18;
const ubyte EQ    = 19;
const ubyte NEQ   = 20;
const ubyte LT    = 21;
const ubyte GT    = 22;
const ubyte NOP   = 23;
const ubyte HALT  = 24;

const long SYS_SPAWN   = 0;
const long SYS_SEND    = 1;
const long SYS_RECV    = 2;
const long SYS_PRINTLN = 3;

const ubyte RETURN_VALUE = 0;
const ubyte RETURN_COPY = 1;

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
                // Remove duplicated whitespaces
                line = replace(line, regex(`\s`), " ");
                line = replace(line, regex(" "), " ");
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
                byte_code ~= PUSH << 3;
                insert(parse!long(parts[0], line), byte_code);
                break;
            case "PUSHS":
                if (operands.length == 0) {
                    throw new ByteCodeError("Invalid instruction " ~ line);
                }
                byte_code ~= PUSHS << 3;
                ubyte[] bytes = cast(ubyte[])toUTF8(operands.strip(`"`));
                insert(cast(int)bytes.length, byte_code);
                byte_code ~= bytes;
                break;
            case "POP":
                assert_no_operands(operands, line);
                byte_code ~= POP << 3;
                break;
            case "DUP":
                assert_no_operands(operands, line);
                byte_code ~= DUP << 3;
                break;
            case "SWAP":
                assert_no_operands(operands, line);
                byte_code ~= SWAP << 3;
                break;
            case "LOAD":
                auto parts = operands.split;
                assert_operands(parts.length, 1, line);
                byte_code ~= add_register(parts[0], LOAD, line);
                break;
            case "STORE":
                auto parts = operands.split;
                assert_operands(parts.length, 1, line);
                byte_code ~= add_register(parts[0], STORE, line);
                break;
            case "ADD":
                assert_no_operands(operands, line);
                byte_code ~= ADD << 3;
                break;
            case "SUB":
                assert_no_operands(operands, line);
                byte_code ~= SUB << 3;
                break;
            case "MUL":
                assert_no_operands(operands, line);
                byte_code ~= MUL << 3;
                break;
            case "DIV":
                assert_no_operands(operands, line);
                byte_code ~= DIV << 3;
                break;
            case "JUMP":
                auto parts = operands.split;
                assert_operands(parts.length, 1, line);
                byte_code ~= JUMP << 3;
                insert(parse!long(parts[0], line), byte_code);
                break;
            case "CJUMP":
                auto parts = operands.split;
                assert_operands(parts.length, 1, line);
                byte_code ~= CJUMP << 3;
                insert(parse!long(parts[0], line), byte_code);
                break;
            case "CALL":
                auto parts = operands.split;
                assert_operands(parts.length, 2, line);
                byte_code ~= CALL << 3;
                insert(parse!int(parts[0], line), byte_code);
                insert(parse!int(parts[1], line), byte_code);
                break;
            case "RET":
                auto parts = operands.split;
                if (parts.length == 0) {
                    byte_code ~= (RET << 3) | RETURN_VALUE;
                } else if (parts.length == 1 && parts[0] == "copy") {
                    byte_code ~= (RET << 3) | RETURN_COPY;
                } else {
                    throw new ByteCodeError("Invalid instruction " ~ line);
                }
                break;
            case "SYS":
                auto parts = operands.split;
                assert_operands(parts.length, 1, line);
                byte_code ~= SYS << 3;
                long sys_name;
                switch(parts[0]) {
                case "spawn":
                    sys_name = SYS_SPAWN;
                    break;
                case "send":
                    sys_name = SYS_SEND;
                    break;
                case "recv":
                    sys_name = SYS_RECV;
                    break;
                case "println":
                    sys_name = SYS_PRINTLN;
                    break;
                default:
                    throw new ByteCodeError("Invalid instruction " ~ line);
                }
                insert(sys_name, byte_code);
                break;
            case "AND":
                assert_no_operands(operands, line);
                byte_code ~= AND << 3;
                break;
            case "OR":
                assert_no_operands(operands, line);
                byte_code ~= OR << 3;
                break;
            case "NOT":
                assert_no_operands(operands, line);
                byte_code ~= NOT << 3;
                break;
            case "EQ":
                assert_no_operands(operands, line);
                byte_code ~= EQ << 3;
                break;
            case "NEQ":
                assert_no_operands(operands, line);
                byte_code ~= NEQ << 3;
                break;
            case "LT":
                assert_no_operands(operands, line);
                byte_code ~= LT << 3;
                break;
            case "GT":
                assert_no_operands(operands, line);
                byte_code ~= GT << 3;
                break;
            case "NOP":
                assert_no_operands(operands, line);
                byte_code ~= NOP << 3;
                break;
            case "HALT":
                assert_no_operands(operands, line);
                byte_code ~= HALT << 3;
                break;
            default:
                throw new ByteCodeError("Invalid instruction " ~ line);
            }
        }

        // Convert labels to byte indices
        long i = 0;
        while (i < byte_code.length) {
            auto opcode = byte_code[i] >> 3;
            if (opcode == PUSH) {
                i += 8;
            } else if (opcode == JUMP || opcode == CJUMP || opcode == CALL) {
                auto label = get!int(&byte_code[i + 1]);
                auto byte_index = jump_table[label];
                set!int(byte_index, &byte_code[i + 1]);
                i += 8;
            } else if (opcode == SYS) {
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
            return cast(ubyte)(opcode << 3) | SP;
        } else if (register == "FP") {
            return cast(ubyte)(opcode << 3) | FP;
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
        case PUSH:
            long value = get!long(&bytes[1]);
            writeln("PUSH " ~ to!string(value));
            return 8;
        case PUSHS:
            auto length = get!int(&bytes[1]);
            auto index = 1 + 4;
            auto byte_string = bytes[index .. index + length + 1];
            writeln("PUSHS \"" ~ cast(string)byte_string ~ "\"");
            return 4 + length;
        case POP:
            writeln("POP");
            break;
        case DUP:
            writeln("DUP");
            break;
        case SWAP:
            writeln("SWAP");
            break;
        case LOAD:
            string register = get_register_string(bytes[0]);
            writeln("LOAD " ~ register);
            break;
        case STORE:
            string register = get_register_string(bytes[0]);
            writeln("STORE " ~ register);
            break;
        case ADD:
            writeln("ADD");
            break;
        case SUB:
            writeln("SUB");
            break;
        case MUL:
            writeln("MUL");
            break;
        case DIV:
            writeln("DIV");
            break;
        case JUMP:
            auto byte_index = get!long(&bytes[1]);
            if (show_labels) {
                auto label = lookup_label(byte_index);
                writeln("JUMP " ~ to!string(label));
            } else {
                writeln("JUMP " ~ to!string(byte_index));
            }
            return 8;
        case CJUMP:
            auto byte_index = get!long(&bytes[1]);
            if (show_labels) {
                auto label = lookup_label(byte_index);
                writeln("CJUMP " ~ to!string(label));
            } else {
                writeln("CJUMP " ~ to!string(byte_index));
            }
            return 8;
        case CALL:
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
        case RET:
            auto return_mode = bytes[0] & 0b00000111;
            if (return_mode == RETURN_COPY) {
                writeln("RET copy");
            } else {
                writeln("RET");
            }
            break;
        case SYS:
            long value = get!long(&bytes[1]);
            writeln("SYS " ~ to!string(value));
            return 8;
        case AND:
            writeln("AND");
            break;
        case OR:
            writeln("OR");
            break;
        case NOT:
            writeln("NOT");
            break;
        case EQ:
            writeln("EQ");
            break;
        case NEQ:
            writeln("NEQ");
            break;
        case LT:
            writeln("LT");
            break;
        case GT:
            writeln("GT");
            break;
        case NOP:
            writeln("NOP");
            break;
        case HALT:
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
        if (register == SP) {
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
