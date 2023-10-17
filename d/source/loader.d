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
     ret    = 14,
     sys    = 15,
     and    = 16,
     or     = 17,
     not    = 18,
     eq     = 19,
     neq    = 20,
     lt     = 21,
     gt     = 22,
     nop    = 23,
     halt   = 24,
     mcall  = 25,
     spawn  = 26,
     mspawn = 27
}

enum SystemCalls : ushort {
    self    = 0,
    send    = 1,
    recv    = 2,
    println = 3,
    display = 4,
    exit    = 5
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
                if (line.empty) {
                    continue;
                }
                // Remove duplicated whitespaces with single blanks
                line = line.replace(regex(r"\s+"), " ");
            }

            // Extract opcode and operands
            auto firstBlank = line.indexOf(" ");
            string opcode;
            string operandsAsString = null;
            string[] operands = null;
            if (firstBlank == -1) {
                opcode = line.strip;
            } else {
                opcode = line[0 .. firstBlank];
                operandsAsString = line[firstBlank + 1 .. $];
                operands = operandsAsString.split;
            }

            switch (opcode) {
            case "label":
                assertOperands(operands.length, 1, line);
                module_.insertLabel(parse!uint(operands[0], line),
                                    cast(uint)byteCode.length);
                continue;
            case "push":
                assertOperands(operands.length, 1, line);
                byteCode ~= Opcodes.push;
                insert(parse!long(operands[0], line), byteCode);
                break;
            case "pushs":
                if (operands.length == 0) {
                    throw new LoaderError("Invalid instruction '" ~ line ~ "'");
                }
                byteCode ~= Opcodes.pushs;
                ubyte[] bytes = cast(ubyte[])toUTF8(operandsAsString.strip(`"`));
                insert(cast(ushort)bytes.length, byteCode);
                byteCode ~= bytes;
                break;
            case "pop":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.pop;
                break;
            case "dup":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.dup;
                break;
            case "swap":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.swap;
                break;
            case "load":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.load;
                break;
            case "store":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.store;
                break;
            case "add":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.add;
                break;
            case "sub":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.sub;
                break;
            case "mul":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.mul;
                break;
            case "div":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.div;
                break;
            case "jump":
                assertOperands(operands.length, 1, line);
                byteCode ~= Opcodes.jump;
                insert(parse!uint(operands[0], line), byteCode);
                break;
            case "cjump":
                assertOperands(operands.length, 1, line);
                byteCode ~= Opcodes.cjump;
                insert(parse!uint(operands[0], line), byteCode);
                break;
            case "call":
                assertOperands(operands.length, 2, line);
                byteCode ~= Opcodes.call;
                insert(parse!uint(operands[0], line), byteCode);
                insert(parse!ubyte(operands[1], line), byteCode);
                break;
            case "mcall":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.mcall;
                break;
            case "ret":
                /// FIXME
                ///////////////assertOperands(operands.length, 0, line);
                ///////////////assertOperands(operands.length, 0, line); or 1
                byteCode ~= Opcodes.ret;
                if (operands.length == 0) {
                    insert(ReturnModes.value, byteCode);
                } else if (operands[0] == "copy") {
                    insert(ReturnModes.copy, byteCode);
                } else {
                    throw new LoaderError("Invalid instruction '" ~ line ~ "'");
                }
                break;
            case "sys":
                assertOperands(operands.length, 1, line);
                byteCode ~= Opcodes.sys;
                uint systemCall;
                switch(operands[0]) {
                case "self":
                    systemCall = SystemCalls.self;
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
                case "exit":
                    systemCall = SystemCalls.exit;
                    break;
                default:
                    throw new LoaderError("Invalid instruction '" ~ line ~ "'");
                }
                insert(systemCall, byteCode);
                break;
            case "and":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.and;
                break;
            case "or":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.or;
                break;
            case "not":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.not;
                break;
            case "eq":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.eq;
                break;
            case "neq":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.neq;
                break;
            case "lt":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.lt;
                break;
            case "gt":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.gt;
                break;
            case "nop":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.nop;
                break;
            case "halt":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.halt;
                break;
            case "spawn":
                assertOperands(operands.length, 2, line);
                byteCode ~= Opcodes.spawn;
                insert(parse!uint(operands[0], line), byteCode);
                insert(parse!ubyte(operands[1], line), byteCode);
                break;
            case "mspawn":
                assertOperands(operands.length, 0, line);
                byteCode ~= Opcodes.mspawn;
                break;
            default:
                throw new LoaderError("Invalid instruction '" ~ line ~ "'");
            }
        }

        // Convert labels to byte indices
        uint byteIndex = module_.startByteIndex;
        while (byteIndex < byteCode.length) {
            auto opcode = byteCode[byteIndex];
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
            } else if (opcode == Opcodes.ret) {
                byteIndex += ubyte.sizeof;
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
            throw new LoaderError("Invalid instruction '" ~ line ~ "'");
        }
    }

    private T parse(T)(string value, string line)
         if (is(T == byte) || is(T == ubyte) || is(T == short) ||
             is(T == ushort) || is(T == int) || is(T == uint) ||
             is(T == long) || is(T == ulong)) {
             try {
                 return to!T(value);
             } catch (ConvException) {
                 throw new LoaderError("Invalid instruction '" ~ line ~ "'");
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

    pragma(inline, true)
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
