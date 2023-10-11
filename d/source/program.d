module program;

import std.stdio : File, writeln;
import std.string : strip, split, indexOf;
import std.conv : to, ConvException;
import std.typecons : Tuple;
import std.algorithm.searching : canFind;
import std.utf: toUTF8;
import std.regex: replace, regex;

struct Registers {
    static const ubyte sp = 0;
    static const ubyte fp = 1;
}

struct Opcodes {
    static const ubyte push  = 0;
    static const ubyte pushs = 1;
    static const ubyte pop   = 2;
    static const ubyte dup   = 3;
    static const ubyte swap  = 4;
    static const ubyte load  = 5;
    static const ubyte store = 6;
    static const ubyte add   = 7;
    static const ubyte sub   = 8;
    static const ubyte mul   = 9;
    static const ubyte div   = 10;
    static const ubyte jump  = 11;
    static const ubyte cjump = 12;
    static const ubyte call  = 13;
    static const ubyte ret   = 14;
    static const ubyte sys   = 15;
    static const ubyte and   = 16;
    static const ubyte or    = 17;
    static const ubyte not   = 18;
    static const ubyte eq    = 19;
    static const ubyte neq   = 20;
    static const ubyte lt    = 21;
    static const ubyte gt    = 22;
    static const ubyte nop   = 23;
    static const ubyte halt  = 24;
}

struct SystemCalls {
    static const long spawn   = 0;
    static const long SEND    = 1;
    static const long recv    = 2;
    static const long println = 3;
    static const long display = 4;
}

struct ReturnModes {
    static const ubyte value = 0;
    static const ubyte copy = 1;
}

class ByteCodeError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
}

struct Program {
    public ubyte[] byteCode;
    public int[int] jumpTable;
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
            auto firstBlank = line.indexOf(" ");
            string opcode;
            string operands = null;
            if (firstBlank == -1) {
                opcode = line.strip;
            } else {
                opcode = line[0 .. firstBlank];
                operands = line[firstBlank + 1 .. $];
            }

            switch (opcode) {
            case "label":
                auto parts = operands.split;
                assertOperands(parts.length, 1, line);
                jumpTable[parse!int(parts[0], line)] = cast(int)byteCode.length;
                continue;
            case "push":
                auto parts = operands.split;
                assertOperands(parts.length, 1, line);
                byteCode ~= Opcodes.push << 3;
                insert(parse!long(parts[0], line), byteCode);
                break;
            case "pushs":
                if (operands.length == 0) {
                    throw new ByteCodeError("Invalid instruction " ~ line);
                }
                byteCode ~= Opcodes.pushs << 3;
                ubyte[] bytes = cast(ubyte[])toUTF8(operands.strip(`"`));
                insert(cast(int)bytes.length, byteCode);
                byteCode ~= bytes;
                break;
            case "pop":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.pop << 3;
                break;
            case "dup":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.dup << 3;
                break;
            case "swap":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.swap << 3;
                break;
            case "load":
                auto parts = operands.split;
                assertOperands(parts.length, 1, line);
                byteCode ~= addRegister(parts[0], Opcodes.load, line);
                break;
            case "store":
                auto parts = operands.split;
                assertOperands(parts.length, 1, line);
                byteCode ~= addRegister(parts[0], Opcodes.store, line);
                break;
            case "add":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.add << 3;
                break;
            case "sub":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.sub << 3;
                break;
            case "mul":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.mul << 3;
                break;
            case "div":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.div << 3;
                break;
            case "jump":
                auto parts = operands.split;
                assertOperands(parts.length, 1, line);
                byteCode ~= Opcodes.jump << 3;
                insert(parse!long(parts[0], line), byteCode);
                break;
            case "cjump":
                auto parts = operands.split;
                assertOperands(parts.length, 1, line);
                byteCode ~= Opcodes.cjump << 3;
                insert(parse!long(parts[0], line), byteCode);
                break;
            case "call":
                auto parts = operands.split;
                assertOperands(parts.length, 2, line);
                byteCode ~= Opcodes.call << 3;
                insert(parse!int(parts[0], line), byteCode);
                insert(parse!int(parts[1], line), byteCode);
                break;
            case "ret":
                auto parts = operands.split;
                if (parts.length == 0) {
                    byteCode ~= (Opcodes.ret << 3) | ReturnModes.value;
                } else if (parts.length == 1 && parts[0] == "copy") {
                    byteCode ~= (Opcodes.ret << 3) | ReturnModes.copy;
                } else {
                    throw new ByteCodeError("Invalid instruction " ~ line);
                }
                break;
            case "sys":
                auto parts = operands.split;
                assertOperands(parts.length, 1, line);
                byteCode ~= Opcodes.sys << 3;
                long systemCall;
                switch(parts[0]) {
                case "spawn":
                    systemCall = SystemCalls.spawn;
                    break;
                case "send":
                    systemCall = SystemCalls.SEND;
                    break;
                case "recv":
                    systemCall = SystemCalls.recv;
                    break;
                case "println":
                    systemCall = SystemCalls.println;
                    break;
                case "display":
                    systemCall = SystemCalls.display;
                    break;
                default:
                    throw new ByteCodeError("Invalid instruction " ~ line);
                }
                insert(systemCall, byteCode);
                break;
            case "and":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.and << 3;
                break;
            case "or":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.or << 3;
                break;
            case "not":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.not << 3;
                break;
            case "eq":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.eq << 3;
                break;
            case "neq":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.neq << 3;
                break;
            case "lt":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.lt << 3;
                break;
            case "gt":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.gt << 3;
                break;
            case "nop":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.nop << 3;
                break;
            case "halt":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.halt << 3;
                break;
            default:
                throw new ByteCodeError("Invalid instruction " ~ line);
            }
        }

        // Convert labels to byte indices
        long i = 0;
        while (i < byteCode.length) {
            auto opcode = byteCode[i] >> 3;
            if (opcode == Opcodes.push) {
                i += 8;
            } else if (opcode == Opcodes.pushs) {
                auto length = get!int(&byteCode[i + 1]);
                i += 4 + length;
            } else if (opcode == Opcodes.jump || opcode == Opcodes.cjump ||
                       opcode == Opcodes.call) {
                auto label = get!int(&byteCode[i + 1]);
                auto byteIndex = jumpTable[label];
                set!int(byteIndex, &byteCode[i + 1]);
                i += 8;
            } else if (opcode == Opcodes.sys) {
                i += 8;
            }
            i++;
        }
    }

    private void assertOperands(long arity, int expectedArity, string line) {
        if (arity != expectedArity) {
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

    private void assertNoOperands(string operands, string line) {
        if (operands.length != 0) {
            throw new ByteCodeError("Invalid instruction " ~ line);
        }
    }

    private ubyte addRegister(string register, ubyte opcode, string line) {
        if (register == "sp") {
            return cast(ubyte)(opcode << 3) | Registers.sp;
        } else if (register == "fp") {
            return cast(ubyte)(opcode << 3) | Registers.fp;
        } else {
            throw new ByteCodeError("Invalid instruction " ~ line);
        }
    }

    public void prettyPrint() {
        long i = 0;
        while (i < byteCode.length) {
            i += 1 + prettyPrint(&byteCode[i], true);
        }
    }

    public long prettyPrint(ubyte* bytes, bool showLabels) {
        switch (bytes[0] >> 3) {
        case Opcodes.push:
            auto value = get!long(&bytes[1]);
            writeln("push " ~ to!string(value));
            return 8;
        case Opcodes.pushs:
            auto length = get!int(&bytes[1]);
            auto index = 1 + 4;
            auto byteString = bytes[index .. index + length];
            writeln("pushs \"" ~ cast(string)byteString ~ "\"");
            return 4 + length;
        case Opcodes.pop:
            writeln("pop");
            break;
        case Opcodes.dup:
            writeln("dup");
            break;
        case Opcodes.swap:
            writeln("swap");
            break;
        case Opcodes.load:
            string register = getRegisterString(bytes[0]);
            writeln("load " ~ register);
            break;
        case Opcodes.store:
            string register = getRegisterString(bytes[0]);
            writeln("store " ~ register);
            break;
        case Opcodes.add:
            writeln("add");
            break;
        case Opcodes.sub:
            writeln("sub");
            break;
        case Opcodes.mul:
            writeln("mul");
            break;
        case Opcodes.div:
            writeln("div");
            break;
        case Opcodes.jump:
            auto byteIndex = get!long(&bytes[1]);
            if (showLabels) {
                auto label = lookupLabel(byteIndex);
                writeln("jump " ~ to!string(label));
            } else {
                writeln("jump " ~ to!string(byteIndex));
            }
            return 8;
        case Opcodes.cjump:
            auto byteIndex = get!long(&bytes[1]);
            if (showLabels) {
                auto label = lookupLabel(byteIndex);
                writeln("cjump " ~ to!string(label));
            } else {
                writeln("cjump " ~ to!string(byteIndex));
            }
            return 8;
        case Opcodes.call:
            auto byteIndex = get!int(&bytes[1]);
            auto arity = get!int(&bytes[5]);
            if (showLabels) {
                auto label = lookupLabel(byteIndex);
                writeln("call " ~ to!string(label) ~ " " ~ to!string(arity));
            } else {
                writeln("call " ~ to!string(byteIndex) ~ " " ~
                        to!string(arity));
            }
            return 8;
        case Opcodes.ret:
            auto returnMode = bytes[0] & 0b00000111;
            if (returnMode == ReturnModes.copy) {
                writeln("ret copy");
            } else {
                writeln("ret");
            }
            break;
        case Opcodes.sys:
            auto value = get!long(&bytes[1]);
            writeln("sys " ~ to!string(value));
            return 8;
        case Opcodes.and:
            writeln("and");
            break;
        case Opcodes.or:
            writeln("or");
            break;
        case Opcodes.not:
            writeln("not");
            break;
        case Opcodes.eq:
            writeln("eq");
            break;
        case Opcodes.neq:
            writeln("neq");
            break;
        case Opcodes.lt:
            writeln("lt");
            break;
        case Opcodes.gt:
            writeln("gt");
            break;
        case Opcodes.nop:
            writeln("nop");
            break;
        case Opcodes.halt:
            writeln("halt");
            break;
        default:
            throw new ByteCodeError("Unknown opcode " ~
                                    to!string(bytes[0] >> 3));
        }

        return 0;
    }

    private long lookupLabel(long byteIndex) {
        foreach (label, possibleByteIndex; jumpTable) {
            if (byteIndex == possibleByteIndex) {
                return label;
            }
        }
        throw new ByteCodeError("Internal Error");
    }

    private string getRegisterString(ubyte instruction) {
        auto register = instruction & 0b00000111;
        if (register == Registers.sp) {
            return "sp";
        } else { // Must be fp
            return "fp";
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
