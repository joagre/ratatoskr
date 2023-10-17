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

class LoaderError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
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
class Loader {
    static string[Opcodes] opcodeToString;
    static Opcodes[string] stringToOpcode;
    static string[SystemCalls] systemCallToString;
    static SystemCalls[string] stringToSystemCall;
    static string[ReturnModes] returnModeToString;
    static ReturnModes[string] stringToReturnMode;

    public ubyte[] byteCode;
    private string loadPath;
    private Module[string] modules;
    private File file;

    this(string loadPath) {
        this.loadPath = loadPath;
    }

    static this() {
        string[Opcodes] opcodeToString = [
            Opcodes.push   : "push",
            Opcodes.pushs  : "pushs",
            Opcodes.pop    : "pop",
            Opcodes.dup    : "dup",
            Opcodes.swap   : "swap",
            Opcodes.load   : "load",
            Opcodes.store  : "store",
            Opcodes.add    : "add",
            Opcodes.sub    : "sub",
            Opcodes.mul    : "mul",
            Opcodes.div    : "div",
            Opcodes.jump   : "jump",
            Opcodes.cjump  : "cjump",
            Opcodes.call   : "call",
            Opcodes.ret    : "ret",
            Opcodes.sys    : "sys",
            Opcodes.and    : "and",
            Opcodes.or     : "or",
            Opcodes.not    : "not",
            Opcodes.eq     : "eq",
            Opcodes.neq    : "neq",
            Opcodes.lt     : "lt",
            Opcodes.gt     : "gt",
            Opcodes.nop    : "nop",
            Opcodes.halt   : "halt",
            Opcodes.mcall  : "mcall",
            Opcodes.spawn  : "spawn",
            Opcodes.mspawn : "mspawn"
        ];

        foreach (opcode, string; opcodeToString) {
            stringToOpcode[string] = opcode;
        }

        string[SystemCalls] systemCallToString = [
            SystemCalls.self    : "self",
            SystemCalls.send    : "send",
            SystemCalls.recv    : "recv",
            SystemCalls.println : "println",
            SystemCalls.display : "display",
            SystemCalls.exit    : "exit"
        ];

        foreach (systemCall, string; systemCallToString) {
            stringToSystemCall[string] = systemCall;
        }

        string[ReturnModes] returnModeToString = [
            ReturnModes.value : "value",
            ReturnModes.copy : "copy"
        ];

        foreach (returnMode, string; returnModeToString) {
            stringToReturnMode[string] = returnMode;
        }
    }

    static void insert(T)(T value, ref ubyte[] bytes) {
        bytes ~= (cast(ubyte*)&value)[0 .. T.sizeof];
    }

    pragma(inline, true)
    static T get(T)(ubyte* bytes) {
        return *cast(T*)bytes;
    }

    static void set(T)(T value, ubyte* bytes) {
        *cast(T*)bytes = value;
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
            string opcodeString;
            string operandsAsString = null;
            string[] operands = null;
            if (firstBlank == -1) {
                opcodeString = line.strip;
            } else {
                opcodeString = line[0 .. firstBlank];
                operandsAsString = line[firstBlank + 1 .. $];
                operands = operandsAsString.split;
            }

            if (opcodeString == "label") {
                assertOperands(operands.length, 1, line);
                module_.insertLabel(parse!uint(operands[0], line),
                                    cast(uint)byteCode.length);
                continue;
            }

            Opcodes opcode;
            if (opcodeString in stringToOpcode) {
                opcode = stringToOpcode[opcodeString];
                byteCode ~= opcode;
            } else {
                throw new LoaderError("Invalid instruction '" ~ line ~ "'");
            }

            switch (opcode) {
            case Opcodes.push:
                assertOperands(operands.length, 1, line);
                insert(parse!long(operands[0], line), byteCode);
                break;
            case Opcodes.pushs:
                if (operandsAsString.length == 0) {
                    throw new LoaderError("Invalid instruction '" ~ line ~ "'");
                }
                ubyte[] bytes = cast(ubyte[])toUTF8(operandsAsString.strip(`"`));
                insert(cast(ushort)bytes.length, byteCode);
                byteCode ~= bytes;
                break;
            case Opcodes.call:
                assertOperands(operands.length, 2, line);
                insert(parse!uint(operands[0], line), byteCode);
                insert(parse!ubyte(operands[1], line), byteCode);
                break;
            case Opcodes.cjump:
                assertOperands(operands.length, 1, line);
                insert(parse!uint(operands[0], line), byteCode);
                break;
            case Opcodes.ret:
                assertOperands(operands.length, [0, 1], line);
                if (operands.length == 0) {
                    insert(ReturnModes.value, byteCode);
                } else if (operands[0] == "copy") {
                    insert(ReturnModes.copy, byteCode);
                } else {
                    throw new LoaderError("Invalid instruction '" ~ line ~ "'");
                }
                break;
            case Opcodes.sys:
                assertOperands(operands.length, 1, line);
                SystemCalls systemCall;
                if (operands[0] in stringToSystemCall) {
                    systemCall = stringToSystemCall[operands[0]];
                } else {
                    throw new LoaderError("Invalid instruction '" ~ line ~ "'");
                }
                insert(systemCall, byteCode);
                break;
            case Opcodes.spawn:
                assertOperands(operands.length, 2, line);
                insert(parse!uint(operands[0], line), byteCode);
                insert(parse!ubyte(operands[1], line), byteCode);
                break;
            case Opcodes.jump:
                assertOperands(operands.length, 1, line);
                insert(parse!uint(operands[0], line), byteCode);
                break;
            default:
                // All is done above!
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
                byteIndex += ushort.sizeof;
            }
            byteIndex++;
        }
    }

    private void assertOperands(ulong arity, ubyte expectedArity, string line) {
        if (arity != expectedArity) {
            throw new LoaderError("Invalid instruction '" ~ line ~ "'");
        }
    }

    private void assertOperands(ulong arity, ubyte[] expectedArities, string line) {
        foreach (expectedArity; expectedArities) {
            if (arity == expectedArity) {
                return;
            }
        }
        throw new LoaderError("Invalid instruction '" ~ line ~ "'");
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
