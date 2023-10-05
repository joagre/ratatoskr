module program;

import std.stdio : File, writeln;
import std.string : strip, split;
import std.conv : to;
import std.regex : regex, replace;
import std.typecons : Tuple;
import std.algorithm.searching : canFind;

const ubyte SP = 0;
const ubyte FP = 1;
const ubyte PC = 2;

const ubyte PUSH   = 0;
const ubyte PUSHR  = 1;
const ubyte POP    = 2;
const ubyte DUP    = 3;
const ubyte SWAP   = 4;
const ubyte LOADR  = 5;
const ubyte STORER = 6;
const ubyte MOVER  = 7;
const ubyte ADD    = 8;
const ubyte SUB    = 9;
const ubyte MUL    = 10;
const ubyte DIV    = 11;
const ubyte JUMP   = 12;
const ubyte CJUMP  = 13;
const ubyte CALL   = 14;
const ubyte RET    = 15;
const ubyte SYS    = 16;
const ubyte AND    = 17;
const ubyte OR     = 18;
const ubyte NOT    = 19;
const ubyte EQ     = 20;
const ubyte NEQ    = 21;
const ubyte LT     = 22;
const ubyte GT     = 23;
const ubyte NOP    = 24;
const ubyte HALT   = 25;

const ulong SYS_SPAWN   = 0;
const ulong SYS_SEND    = 1;
const ulong SYS_RECV    = 2;
const ulong SYS_RAND    = 3;
const ulong SYS_SLEEP   = 4;
const ulong SYS_PRINTLN = 5;

const ubyte LONG = 0;
const ubyte ULONG = 1;
const ubyte STRING = 2;

class ByteCodeError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
}

struct Program {
    public ubyte[] byte_code;
    public ulong[ulong] jump_table;
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
            }

            // Remove comments
            line = line.split(";")[0];
            // Remove duplicated whitespace and return with a single blankspace
            line = replace(line, regex(`\s+`), " ");

            if (line.length == 0) {
                continue;
            }

            immutable auto parts = line.strip.split;

            switch (parts[0]) {
            case "LABEL":
                jump_table[to!ulong(parts[1])] = byte_code.length;
                continue;
            case "PUSH":
                byte_code ~= PUSH << 3;
                insert_long(to!long(parts[1]), byte_code);
                break;
            case "PUSHR":
                byte_code ~= add_register(parts[1], PUSHR);
                break;
            case "POP":
                byte_code ~= POP << 3;
                break;
            case "DUP":
                byte_code ~= DUP << 3;
                break;
            case "SWAP":
                byte_code ~= SWAP << 3;
                break;
            case "LOADR":
                byte_code ~= add_register(parts[1], ["SP", "FP"], LOADR);
                break;
            case "STORER":
                byte_code ~= add_register(parts[1], ["SP", "FP"], STORER);
                break;
            case "MOVER":
                byte_code ~= add_register(parts[1], MOVER);
                break;
            case "ADD":
                byte_code ~= ADD << 3;
                break;
            case "SUB":
                byte_code ~= SUB << 3;
                break;
            case "MUL":
                byte_code ~= MUL << 3;
                break;
            case "DIV":
                byte_code ~= DIV << 3;
                break;
            case "JUMP":
                byte_code ~= JUMP << 3;
                insert_ulong(to!ulong(parts[1]), byte_code);
                break;
            case "CJUMP":
                byte_code ~= CJUMP << 3;
                insert_ulong(to!ulong(parts[1]), byte_code);
                break;
            case "CALL":
                byte_code ~= CALL << 3;
                insert_ulong(to!ulong(parts[1]), byte_code);
                break;
            case "RET":
                byte_code ~= RET << 3;
                break;
            case "SYS":
                byte_code ~= SYS << 3;
                insert_sys_name(parts[1], byte_code);
                break;
            case "AND":
                byte_code ~= AND << 3;
                break;
            case "OR":
                byte_code ~= OR << 3;
                break;
            case "NOT":
                byte_code ~= NOT << 3;
                break;
            case "EQ":
                byte_code ~= EQ << 3;
                break;
            case "NEQ":
                byte_code ~= NEQ << 3;
                break;
            case "LT":
                byte_code ~= LT << 3;
                break;
            case "GT":
                byte_code ~= GT << 3;
                break;
            case "NOP":
                byte_code ~= NOP << 3;
                break;
            case "HALT":
                byte_code ~= HALT << 3;
                break;
            default:
                throw new ByteCodeError("Invalid instruction " ~ parts[0]);
            }
        }

        // Convert labels to byte indices
        ulong i = 0;
        while (i < byte_code.length) {
            auto opcode = byte_code[i] >> 3;
            if (opcode == PUSH) {
                i += 8;
            } else if (opcode == JUMP || opcode == CJUMP || opcode == CALL) {
                auto label = get_ulong(&byte_code[i + 1]);
                auto byte_index = jump_table[label];
                set_ulong(byte_index, &byte_code[i + 1]);
                i += 8;
            } else if (opcode == SYS) {
                i += 8;
            }
            i++;
        }
    }

    public void insert_long(long value, ref ubyte[] bytes) {
        for (int i = 0; i < 8; i++){
            bytes ~= cast(ubyte)(value & 0xFF);
            value >>= 8;
        }
    }

    public void insert_ulong(ulong value, ref ubyte[] bytes) {
        for (int i = 0; i < 8; i++){
            bytes ~= cast(ubyte)(value & 0xFF);
            value >>= 8;
        }
    }

    private ubyte add_register(string s, ubyte opcode) {
        return add_register(s, ["SP", "FP", "PC"],  opcode);
    }

    private ubyte add_register(string s, string[] valid_registers,
                               ubyte opcode) {
        if (!valid_registers.canFind(s)) {
            throw new ByteCodeError("Invalid register " ~ s);
        }
        if (s == "SP") {
            return cast(ubyte)(opcode << 3) | SP;
        } else if (s == "FP") {
            return cast(ubyte)(opcode << 3) | FP;
        } else if (s == "PC") {
            return cast(ubyte)(opcode << 3) | PC;
        } else {
            throw new ByteCodeError("Invalid register " ~ s);
        }
    }

    private void insert_sys_name(string s, ref ubyte[] bytes) {
        ulong sys_name;
        if (s == "spawn") {
            sys_name = SYS_SPAWN;
        } else if (s == "send") {
            sys_name = SYS_SEND;
        } else if (s ==  "recv") {
            sys_name = SYS_RECV;
        } else if (s == "rand") {
            sys_name = SYS_RAND;
        } else if (s == "sleep") {
            sys_name = SYS_SLEEP;
        } else if (s == "println") {
            sys_name = SYS_PRINTLN;
        } else {
            throw new ByteCodeError("Unknown system function " ~ s);
        }
        insert_ulong(sys_name, bytes);
    }

    public void pretty_print() {
        ulong i = 0;
        while (i < byte_code.length) {
            i += 1 + pretty_print(&byte_code[i], true);
        }
    }

    public ulong pretty_print(ubyte* bytes, bool show_labels) {
        switch (bytes[0] >> 3) {
        case PUSH:
            long value = get_long(&bytes[1]);
            writeln("PUSH " ~ to!string(value));
            return 8;
        case PUSHR:
            string register = get_register_string(bytes[0]);
            writeln("PUSHR " ~ register);
            break;
        case POP:
            writeln("POP");
            break;
        case DUP:
            writeln("DUP");
            break;
        case SWAP:
            writeln("SWAP");
            break;
        case LOADR:
            string register = get_register_string(bytes[0]);
            writeln("LOADR " ~ register);
            break;
        case STORER:
            string register = get_register_string(bytes[0]);
            writeln("STORER " ~ register);
            break;
        case MOVER:
            string register = get_register_string(bytes[0]);
            writeln("MOVER " ~ register);
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
            auto byte_index = get_ulong(&bytes[1]);
            if (show_labels) {
                auto label = lookup_label(byte_index);
                writeln("JUMP " ~ to!string(label));
            } else {
                writeln("JUMP " ~ to!string(byte_index));
            }
            return 8;
        case CJUMP:
            auto byte_index = get_ulong(&bytes[1]);
            if (show_labels) {
                auto label = lookup_label(byte_index);
                writeln("CJUMP " ~ to!string(label));
            } else {
                writeln("CJUMP " ~ to!string(byte_index));
            }
            return 8;
        case CALL:
            auto byte_index = get_ulong(&bytes[1]);
            if (show_labels) {
                auto label = lookup_label(byte_index);
                writeln("CALL " ~ to!string(label));
            } else {
                writeln("CALL " ~ to!string(byte_index));
            }
            return 8;
        case RET:
            writeln("RET");
            break;
        case SYS:
            ulong value = get_ulong(&bytes[1]);
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

    private ulong lookup_label(ulong byte_index) {
        foreach (label, possible_byte_index; jump_table) {
            if (byte_index == possible_byte_index) {
                return label;
            }
        }
        throw new ByteCodeError("Internal Error");
    }

    private string get_register_string(ubyte instruction) {
        ubyte register = instruction & 0b00000111;
        if (register == SP) {
            return "SP";
        } else if (register == FP) {
            return "FP";
        } else { // Must be PC
            return "PC";
        }
    }

}

long get_long(ubyte* bytes) {
    ulong value = 0;
    for (int i = 0; i < 8; i++) {
        value |= (cast(ulong)bytes[i]) << (i * 8);
    }
    if ((value & (1UL << 63)) != 0) {
        return -1 - (cast(long) (~value));
    } else {
        return cast(long) value;
    }
}

ulong get_ulong(ubyte* bytes) {
    ulong value = 0;
    for (int i = 0; i < 8; i++) {
        value |= (cast(ulong)bytes[i]) << (i * 8);
    }
    return value;
}

void set_ulong(ulong value, ubyte* bytes) {
    for (int i = 0; i < 8; i++) {
        bytes[i] = cast(ubyte)(value & 0xFF);
        value >>= 8;
    }
}

/*

KEEP: If I at some time feel the need to tag values on the stack

Tuple!(ubyte, ulong) untag_ulong(ulong tagged_value) {
    return Tuple!(ubyte, ulong)
                (cast(ubyte)((tagged_value >> 61) & 0x7),
                tagged_value & ((1UL << 61) - 1));
}

ulong tag_ulong(ulong untagged_value, ubyte tag) {
    return untagged_value | (cast(ulong)tag << 61);
}

long ulong2long(ulong unsigned_value) {
    if ((unsigned_value & (1L << 60)) != 0) {
        return cast(long)(unsigned_value | ~((1L << 61) - 1));
    } else {
        return cast(long)unsigned_value;
    }
}

ulong long2ulong(long signed_value) {
    return cast(ulong)signed_value & ((1UL << 61) - 1);
}

unittest {
    void insert_ulong(ulong value, ref ubyte[] bytes) {
        for (int i = 0; i < 8; i++){
            bytes ~= cast(ubyte)(value & 0xFF);
            value >>= 8;
        }
    }
    ubyte[] byte_code;
    long signed = to!long("-455");
    ulong unsigned = long2ulong(signed);
    insert_ulong(unsigned, byte_code);
    ulong a = get_ulong(&byte_code[0]);
    long b = ulong2long(a);
    assert(b == signed);
}

*/
