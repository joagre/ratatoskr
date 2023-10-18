module loader;

import std.stdio : File, writeln, writef, writefln;
import std.string : strip, split, indexOf;
import std.conv : to, ConvException;
import std.typecons : Tuple;
import std.algorithm.searching : canFind;
import std.utf: toUTF8;
import std.regex: replace, regex, matchFirst;
import std.path: buildPath;
import std.range: empty;
import std.file: exists;

import instructions;
import prettyprint;

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
            if (opcodeString in Instructions.stringToOpcode) {
                opcode = Instructions.stringToOpcode[opcodeString];
                byteCode ~= opcode;
            } else {
                throw new LoaderError("Invalid instruction '" ~ line ~ "'");
            }

            switch (opcode) {
                // Register machine instructions
            case Opcodes.jmprnze:
                assertOperands(operands.length, 2, line);
                auto register = parseRegister(operands[0], line);
                Instructions.insert(register, byteCode);
                Instructions.insert(parse!uint(operands[1], line), byteCode);
                break;




            // Stack machone instructions
            case Opcodes.push:
                assertOperands(operands.length, 1, line);
                Instructions.insert(parse!long(operands[0], line), byteCode);
                break;
            case Opcodes.pushs:
                if (operandsAsString.length == 0) {
                    throw new LoaderError("Invalid instruction '" ~ line ~ "'");
                }
                ubyte[] bytes = cast(ubyte[])toUTF8(operandsAsString.strip(`"`));
                Instructions.insert(cast(ushort)bytes.length, byteCode);
                byteCode ~= bytes;
                break;
            case Opcodes.call:
                assertOperands(operands.length, 2, line);
                Instructions.insert(parse!uint(operands[0], line), byteCode);
                Instructions.insert(parse!ubyte(operands[1], line), byteCode);
                break;
            case Opcodes.cjump:
                assertOperands(operands.length, 1, line);
                Instructions.insert(parse!uint(operands[0], line), byteCode);
                break;
            case Opcodes.ret:
                assertOperands(operands.length, [0, 1], line);
                if (operands.length == 0) {
                    Instructions.insert(ReturnModes.value, byteCode);
                } else if (operands[0] == "copy") {
                    Instructions.insert(ReturnModes.copy, byteCode);
                } else {
                    throw new LoaderError("Invalid instruction '" ~ line ~ "'");
                }
                break;
            case Opcodes.sys:
                assertOperands(operands.length, 1, line);
                SystemCalls systemCall;
                if (operands[0] in Instructions.stringToSystemCall) {
                    systemCall = Instructions.stringToSystemCall[operands[0]];
                } else {
                    throw new LoaderError("Invalid instruction '" ~ line ~ "'");
                }
                Instructions.insert(systemCall, byteCode);
                break;
            case Opcodes.spawn:
                assertOperands(operands.length, 2, line);
                Instructions.insert(parse!uint(operands[0], line), byteCode);
                Instructions.insert(parse!ubyte(operands[1], line), byteCode);
                break;
            case Opcodes.jump:
                assertOperands(operands.length, 1, line);
                Instructions.insert(parse!uint(operands[0], line), byteCode);
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
                auto length = Instructions.get!ushort(&byteCode[byteIndex + 1]);
                byteIndex += ushort.sizeof + length;
            } else if (opcode == Opcodes.jump || opcode == Opcodes.cjump) {
                auto label = Instructions.get!uint(&byteCode[byteIndex + 1]);
                auto labelByteIndex = module_.lookupByteIndex(label);
                Instructions.set!uint(labelByteIndex, &byteCode[byteIndex + 1]);
                byteIndex += uint.sizeof;
            } else if (opcode == Opcodes.call) {
                auto label = Instructions.get!uint(&byteCode[byteIndex + 1]);
                auto labelByteIndex = module_.lookupByteIndex(label);
                Instructions.set!uint(labelByteIndex, &byteCode[byteIndex + 1]);
                byteIndex += uint.sizeof + ubyte.sizeof;
            } else if (opcode == Opcodes.ret) {
                byteIndex += ubyte.sizeof;
            } else if (opcode == Opcodes.spawn) {
                auto label = Instructions.get!uint(&byteCode[byteIndex + 1]);
                auto labelByteIndex = module_.lookupByteIndex(label);
                Instructions.set!uint(labelByteIndex, &byteCode[byteIndex + 1]);
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

    private ubyte parseRegister(string registerString, string line) {
        auto match = matchFirst(registerString, regex(`^r([0-9]{1,3})`));
        if (match) {
            auto register = parse!ubyte(match.captures[1], line);
            if (register >= 0 && register <= 63) {
                return register;
            }
        }
        throw new LoaderError("Invalid instruction '" ~ line ~ "'");
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
