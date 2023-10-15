module loader;

import std.stdio : File, writeln, writef, writefln;
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

enum Registers : ubyte {
    sp = 0,
    fp = 1
}

enum Opcodes : ubyte {
     push   = 0,
     pushs  = 1,
     pop    = 2,
     dup    = 3,
     swap   = 4,
     load   = 5,
     store  = 6,
     add    = 7,
     sub    = 8,
     mul    = 9,
     div    = 10,
     jump   = 11,
     cjump  = 12,
     call   = 13,
     mcall  = 14,
     ret    = 15,
     sys    = 16,
     and    = 17,
     or     = 18,
     not    = 19,
     eq     = 20,
     neq    = 21,
     lt     = 22,
     gt     = 23,
     nop    = 24,
     halt   = 25,
     spawn  = 26,
     mspawn = 27
}

enum SystemCalls : ushort {
    send    = 0,
    recv    = 1,
    println = 2,
    display = 3,
    exit    = 4
}

enum ReturnModes : ubyte {
    value = 0,
    copy = 1
}

class LoaderError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
}

const ubyte OPCODE_OPERAND_MASK = 0b00000111;
const ubyte OPCODE_BITS = 3;

class Loader {
    public ubyte[] byteCode;
    private string loadPath;
    private Module[string] modules;
    private File file;

    this(string loadPath) {
        this.loadPath = loadPath;
    }

    private void generateByteCode(Module module_) {
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
                byteCode ~= Opcodes.push << OPCODE_BITS;
                insert(parse!long(parts[0], line), byteCode);
                break;
            case "pushs":
                if (operands.length == 0) {
                    throw new LoaderError("Invalid instruction " ~ line);
                }
                byteCode ~= Opcodes.pushs << OPCODE_BITS;
                ubyte[] bytes = cast(ubyte[])toUTF8(operands.strip(`"`));
                insert(cast(ushort)bytes.length, byteCode);
                byteCode ~= bytes;
                break;
            case "pop":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.pop << OPCODE_BITS;
                break;
            case "dup":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.dup << OPCODE_BITS;
                break;
            case "swap":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.swap << OPCODE_BITS;
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
                byteCode ~= Opcodes.add << OPCODE_BITS;
                break;
            case "sub":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.sub << OPCODE_BITS;
                break;
            case "mul":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.mul << OPCODE_BITS;
                break;
            case "div":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.div << OPCODE_BITS;
                break;
            case "jump":
                auto parts = operands.split;
                assertOperands(parts.length, 1, line);
                byteCode ~= Opcodes.jump << OPCODE_BITS;
                insert(parse!uint(parts[0], line), byteCode);
                break;
            case "cjump":
                auto parts = operands.split;
                assertOperands(parts.length, 1, line);
                byteCode ~= Opcodes.cjump << OPCODE_BITS;
                insert(parse!uint(parts[0], line), byteCode);
                break;
            case "call":
                auto parts = operands.split;
                assertOperands(parts.length, 2, line);
                byteCode ~= Opcodes.call << OPCODE_BITS;
                insert(parse!uint(parts[0], line), byteCode);
                insert(parse!ubyte(parts[1], line), byteCode);
                break;
            case "mcall":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.mcall << OPCODE_BITS;
                break;
            case "ret":
                auto parts = operands.split;
                if (parts.length == 0) {
                    byteCode ~=
                        (Opcodes.ret << OPCODE_BITS) | ReturnModes.value;
                } else if (parts.length == 1 && parts[0] == "copy") {
                    byteCode ~= (Opcodes.ret << OPCODE_BITS) | ReturnModes.copy;
                } else {
                    throw new LoaderError("Invalid instruction " ~ line);
                }
                break;
            case "sys":
                auto parts = operands.split;
                assertOperands(parts.length, 1, line);
                byteCode ~= Opcodes.sys << OPCODE_BITS;
                uint systemCall;
                switch(parts[0]) {
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
                case "exit":
                    systemCall = SystemCalls.exit;
                    break;
                default:
                    throw new LoaderError("Invalid instruction " ~ line);
                }
                insert(systemCall, byteCode);
                break;
            case "and":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.and << OPCODE_BITS;
                break;
            case "or":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.or << OPCODE_BITS;
                break;
            case "not":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.not << OPCODE_BITS;
                break;
            case "eq":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.eq << OPCODE_BITS;
                break;
            case "neq":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.neq << OPCODE_BITS;
                break;
            case "lt":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.lt << OPCODE_BITS;
                break;
            case "gt":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.gt << OPCODE_BITS;
                break;
            case "nop":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.nop << OPCODE_BITS;
                break;
            case "halt":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.halt << OPCODE_BITS;
                break;
            case "spawn":
                auto parts = operands.split;
                assertOperands(parts.length, 2, line);
                byteCode ~= Opcodes.spawn << OPCODE_BITS;
                insert(parse!uint(parts[0], line), byteCode);
                insert(parse!ubyte(parts[1], line), byteCode);
                break;
            case "mspawn":
                assertNoOperands(operands, line);
                byteCode ~= Opcodes.mspawn << OPCODE_BITS;
                break;
            default:
                throw new LoaderError("Invalid instruction " ~ line);
            }
        }

        // Convert labels to byte indices
        uint byteIndex = module_.startByteIndex;
        while (byteIndex < byteCode.length) {
            auto opcode = byteCode[byteIndex] >> OPCODE_BITS;
            if (opcode == Opcodes.push) {
                byteIndex += long.sizeof;
            } else if (opcode == Opcodes.pushs) {
                auto length = get!ushort(&byteCode[byteIndex + 1]);
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
            } else if (opcode == Opcodes.spawn) {
                auto label = get!uint(&byteCode[byteIndex + 1]);
                auto labelByteIndex = module_.lookupByteIndex(label);
                set!uint(labelByteIndex, &byteCode[byteIndex + 1]);
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
            return cast(ubyte)(opcode << OPCODE_BITS) | Registers.sp;
        } else if (register == "fp") {
            return cast(ubyte)(opcode << OPCODE_BITS) | Registers.fp;
        } else {
            throw new LoaderError("Invalid instruction " ~ line);
        }
    }

    public bool isModuleLoaded(string moduleName) {
        return (moduleName in modules) != null;
    }

    public uint lookupByteIndex(string moduleName, uint label) {
        auto module_ = moduleName in modules;
        return module_.lookupByteIndex(label);
    }

    public void loadPOSMCode(string moduleName) {
        auto filename = buildPath(loadPath, moduleName ~ ".posm");
        if (!exists(filename)) {
            throw new LoaderError(filename ~  " can not be found");
        }
        file = File(filename, "r");
        Module module_ = new Module(cast(uint)byteCode.length);
        try {
            generateByteCode(module_);
        } finally {
            file.close;
        }
        module_.stopByteIndex = cast(uint)byteCode.length - 1;
        modules[moduleName] = module_;
    }

    public void prettyPrint() {
        uint byteIndex = 0;
        while (byteIndex < byteCode.length) {
            writef("%d: ", byteIndex);
            byteIndex += 1 + PrettyPrint.printInstruction(&byteCode[byteIndex]);
        }
    }

    public void prettyPrint(string moduleName) {
        auto module_ = modules[moduleName];
        auto byteIndex = module_.startByteIndex;
        while (byteIndex < module_.stopByteIndex + 1) {
            writef("%d: ", byteIndex);
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

class Module {
    public uint startByteIndex;
    public uint stopByteIndex;
    private uint[uint] jumpTable;

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
}
