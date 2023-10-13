module loader;

import std.stdio : File, writeln;
import std.string : strip, split, indexOf;
import std.conv : to, ConvException;
import std.typecons : Tuple;
import std.algorithm.searching : canFind;
import std.utf: toUTF8;
import std.regex: replace, regex;
import std.path: buildPath;
import std.range: empty;
import std.file: exists;
import prettyprint;

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
    static const ubyte mcall = 14;
    static const ubyte ret   = 15;
    static const ubyte sys   = 16;
    static const ubyte and   = 17;
    static const ubyte or    = 18;
    static const ubyte not   = 19;
    static const ubyte eq    = 20;
    static const ubyte neq   = 21;
    static const ubyte lt    = 22;
    static const ubyte gt    = 23;
    static const ubyte nop   = 24;
    static const ubyte halt  = 25;
}

struct SystemCalls {
    static const ushort spawn   = 0;
    static const ushort send    = 1;
    static const ushort recv    = 2;
    static const ushort println = 3;
    static const ushort display = 4;
}

struct ReturnModes {
    static const ubyte value = 0;
    static const ubyte copy = 1;
}

class LoaderError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
}

struct Loader {
    public ubyte[] byteCode;
    private string loadPath;
    private Module[string] modules;
    private File file;

    this(string loadPath) {
        this.loadPath = loadPath;
    }

    public bool isModuleLoaded(string moduleName) {
        return (moduleName in modules) != null;
    }

    public void loadPOSMCode(string moduleName) {
        auto filename = buildPath(loadPath, moduleName ~ ".posm");
        if (!exists(filename)) {
            throw new LoaderError(filename ~  " can not be found");
        }
        file = File(filename, "r");
        Module module_ = Module(cast(uint)byteCode.length);
        try {
            generateByteCode(module_);
        } finally {
            file.close;
        }
        module_.stopByteIndex = cast(uint)byteCode.length - 1;
        modules[moduleName] = module_;
    }

    private void generateByteCode(ref Module module_) {
        while (!file.eof) {
            string line = file.readln.strip;

            if (line.empty) {
                continue;
            } else {
                // Remove comments
                line = line.split(";")[0].strip;
                // Remove duplicated whitespaces with single blanks
                line = line.replace(regex(r"\s+"), " ");
                if (line.empty) {
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
                module_.insertLabel(parse!uint(parts[0], line),
                                    cast(uint)byteCode.length);
                continue;
            case "push":
                auto parts = operands.split;
                assertOperands(parts.length, 1, line);
                byteCode ~= Opcodes.push << 3;
                insert(parse!long(parts[0], line), byteCode);
                break;
            case "pushs":
                if (operands.length == 0) {
                    throw new LoaderError("Invalid instruction " ~ line);
                }
                byteCode ~= Opcodes.pushs << 3;
                ubyte[] bytes = cast(ubyte[])toUTF8(operands.strip(`"`));
                insert(cast(ushort)bytes.length, byteCode);
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
                insert(parse!uint(parts[0], line), byteCode);
                break;
            case "cjump":
                auto parts = operands.split;
                assertOperands(parts.length, 1, line);
                byteCode ~= Opcodes.cjump << 3;
                insert(parse!uint(parts[0], line), byteCode);
                break;
            case "call":
                auto parts = operands.split;
                assertOperands(parts.length, 2, line);
                byteCode ~= Opcodes.call << 3;
                insert(parse!uint(parts[0], line), byteCode);
                insert(parse!ubyte(parts[1], line), byteCode);
                break;
            case "mcall":
                auto parts = operands.split;
                assertOperands(parts.length, 2, line);
                byteCode ~= Opcodes.mcall << 3;
                insert(parse!uint(parts[0], line), byteCode);
                insert(parse!ubyte(parts[1], line), byteCode);
                break;
            case "ret":
                auto parts = operands.split;
                if (parts.length == 0) {
                    byteCode ~= (Opcodes.ret << 3) | ReturnModes.value;
                } else if (parts.length == 1 && parts[0] == "copy") {
                    byteCode ~= (Opcodes.ret << 3) | ReturnModes.copy;
                } else {
                    throw new LoaderError("Invalid instruction " ~ line);
                }
                break;
            case "sys":
                auto parts = operands.split;
                assertOperands(parts.length, 1, line);
                byteCode ~= Opcodes.sys << 3;
                uint systemCall;
                switch(parts[0]) {
                case "spawn":
                    systemCall = SystemCalls.spawn;
                    break;
                case "send":
                    systemCall = SystemCalls.send;
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
                    throw new LoaderError("Invalid instruction " ~ line);
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
                throw new LoaderError("Invalid instruction " ~ line);
            }
        }

        // Convert labels to byte indices
        uint byteIndex = module_.startByteIndex;
        while (byteIndex < byteCode.length) {
            auto opcode = byteCode[byteIndex] >> 3;
            if (opcode == Opcodes.push) {
                byteIndex += long.sizeof;
            } else if (opcode == Opcodes.pushs) {
                auto length = get!uint(&byteCode[byteIndex + 1]);
                byteIndex += ushort.sizeof + length;
            } else if (opcode == Opcodes.jump || opcode == Opcodes.cjump) {
                auto label = get!uint(&byteCode[byteIndex + 1]);
                auto labelByteIndex = module_.lookupByteIndex(label);
                set!uint(labelByteIndex, &byteCode[byteIndex + 1]);
                byteIndex += uint.sizeof;
            } else if (opcode == Opcodes.call) {
                auto label = get!uint(&byteCode[byteIndex + 1]);
                auto labelByteIndex = module_.lookupByteIndex(label);
                set!uint(labelByteIndex, &byteCode[byteIndex + 1]);
                byteIndex += uint.sizeof + ubyte.sizeof;
            } else if (opcode == Opcodes.mcall) {
                // NOTE: These are patched dynamically in runtime
                byteIndex += uint.sizeof + ubyte.sizeof;
            } else if (opcode == Opcodes.sys) {
                byteIndex += uint.sizeof;
            }
            byteIndex++;
        }
    }

    private void assertOperands(ulong arity, ubyte expectedArity, string line) {
        if (arity != expectedArity) {
            throw new LoaderError("Invalid instruction " ~ line);
        }
    }

    private T parse(T)(string value, string line)
         if (is(T == byte) || is(T == ubyte) || is(T == short) ||
             is(T == ushort) || is(T == int) || is(T == uint) ||
             is(T == long) || is(T == ulong)) {
             try {
                 return to!T(value);
             } catch (ConvException) {
                 throw new LoaderError("Invalid instruction " ~ line);
             }
         }

    private void assertNoOperands(string operands, string line) {
        if (operands.length != 0) {
            throw new LoaderError("Invalid instruction " ~ line);
        }
    }

    private ubyte addRegister(string register, ubyte opcode, string line) {
        if (register == "sp") {
            return cast(ubyte)(opcode << 3) | Registers.sp;
        } else if (register == "fp") {
            return cast(ubyte)(opcode << 3) | Registers.fp;
        } else {
            throw new LoaderError("Invalid instruction " ~ line);
        }
    }

    public void prettyPrint() {
        uint byteIndex = 0;
        while (byteIndex < byteCode.length) {
            byteIndex += 1 + PrettyPrint.printInstruction(&byteCode[byteIndex]);
        }
    }

    public void prettyPrint(string moduleName) {
        auto module_ = modules[moduleName];
        auto byteIndex = module_.startByteIndex;
        while (byteIndex < module_.stopByteIndex + 1) {
            byteIndex += 1 + PrettyPrint.printInstruction(&byteCode[byteIndex]);
        }
    }

    static public void insert(T)(T value, ref ubyte[] bytes) {
        bytes ~= (cast(ubyte*)&value)[0 .. T.sizeof];
    }

    static public T get(T)(ubyte* bytes) {
        return *cast(T*)bytes;
    }

    static public void set(T)(T value, ubyte* bytes) {
        *cast(T*)bytes = value;
    }
}

struct Module {
    public uint startByteIndex;
    public uint stopByteIndex;
    private uint[uint] jumpTable;
    //ExternalCalls[] externalCalls;

    this(uint startByteIndex) {
        this.startByteIndex = startByteIndex;
    }

    public void insertLabel(uint label, uint byteIndex) {
        jumpTable[label] = byteIndex;
    }

    public uint lookupByteIndex(uint label) {
        return jumpTable[label];
    }

    public uint lookupLabel(uint byteIndex) {
        foreach (label, possibleByteIndex; jumpTable) {
            if (byteIndex == possibleByteIndex) {
                return label;
            }
        }
        throw new LoaderError("Internal Error");
    }

    //unresolvedCallLabels?

    //public reverseLookup?
}

//struct ExternalCalls {
//    string moduleName;
//    uint label;
//    uint byteCodeIndex;
//    bool resolved = false;
//}
