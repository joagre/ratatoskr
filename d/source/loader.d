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
import job;

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
                auto label = parse!uint(operands[1], line);
                Instructions.insert(register, byteCode);
                Instructions.insert(label, byteCode);
            break;
            case Opcodes.jmpringt:
                //rilParse(operands,




                assertOperands(operands.length, 3, line);
                auto register = parseRegister(operands[0], line);
                auto value = parseImmediateValue(operands[1], line);
                auto label = parse!uint(operands[2], line);
                Instructions.insert(register, byteCode);
                Instructions.insert(value, byteCode);
                Instructions.insert(label, byteCode);
                break;
            case Opcodes.subrri:
                assertOperands(operands.length, 3, line);
                auto firstRegister = parseRegister(operands[0], line);
                auto secondRegister = parseRegister(operands[1], line);
                auto value = parseImmediateValue(operands[2], line);
                Instructions.insert(firstRegister, byteCode);
                Instructions.insert(secondRegister, byteCode);
                Instructions.insert(value, byteCode);
                break;

            case Opcodes.subrsi:
                assertOperands(operands.length, 3, line);
                auto register = parseRegister(operands[0], line);
                auto stackOffset = parseStackOffset(operands[1], line);
                auto value = parseImmediateValue(operands[2], line);
                Instructions.insert(register, byteCode);
                Instructions.insert(stackOffset, byteCode);
                Instructions.insert(value, byteCode);
                break;


            case Opcodes.addrri:
                assertOperands(operands.length, 3, line);
                auto firstRegister = parseRegister(operands[0], line);
                auto secondRegister = parseRegister(operands[1], line);
                auto value = parseImmediateValue(operands[2], line);
                Instructions.insert(firstRegister, byteCode);
                Instructions.insert(secondRegister, byteCode);
                Instructions.insert(value, byteCode);
                break;
            case Opcodes.loadri:
                assertOperands(operands.length, 2, line);
                auto register = parseRegister(operands[0], line);
                auto value = parseImmediateValue(operands[1], line);
                Instructions.insert(register, byteCode);
                Instructions.insert(value, byteCode);
                break;
            case Opcodes.pushr:
                assertOperands(operands.length, 1, line);
                auto register = parseRegister(operands[0], line);
                Instructions.insert(register, byteCode);
                break;
            case Opcodes.loadrs:
                assertOperands(operands.length, 2, line);
                auto register = parseRegister(operands[0], line);
                auto stackOffset = parseStackOffset(operands[1], line);
                Instructions.insert(register, byteCode);
                // NOTE: The stack offset is relative to the current
                // FP. The current FP points to the return address and
                // after the return address comes the previous
                // FP. Compensate for these two stack positions.
                Instructions.insert(stackOffset, byteCode);
                break;
            case Opcodes.loadrr:
                assertOperands(operands.length, 2, line);
                auto firstRegister = parseRegister(operands[0], line);
                auto secondRegister = parseRegister(operands[1], line);
                Instructions.insert(firstRegister, byteCode);
                Instructions.insert(secondRegister, byteCode);
                break;









            case Opcodes.rcall:
                assertOperands(operands.length, 1, line);
                auto label = parse!uint(operands[0], line);
                Instructions.insert(label, byteCode);
                break;
            case Opcodes.jmp:
                assertOperands(operands.length, 1, line);
                auto label = parse!uint(operands[0], line);
                Instructions.insert(label, byteCode);
                break;








            // Stack machine instructions
            case Opcodes.push:
                assertOperands(operands.length, 1, line);
                Instructions.insert(parse!long(operands[0], line), byteCode);
                break;
            case Opcodes.pushs:
                if (operandsAsString.length == 0) {
                    throw new LoaderError("Invalid instruction '" ~ line ~ "'");
                }
                ubyte[] bytes =
                    cast(ubyte[])toUTF8(operandsAsString.strip(`"`));
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

        // Resolve labels to addresses
        uint address = module_.startAddress;
        while (address < byteCode.length) {
            auto opcode = byteCode[address];
            // Register machine instructions
            if (opcode == Opcodes.jmprnze) {
                auto label = Instructions.get!uint(&byteCode[address + 1 + ubyte.sizeof]);
                auto labelAddress = module_.lookupAddress(label);
                Instructions.set!uint(labelAddress, &byteCode[address + 1 + ubyte.sizeof]);
                address += ubyte.sizeof + uint.sizeof;
            } else if (opcode == Opcodes.jmpringt) {
                auto label = Instructions.get!uint(&byteCode[address + 1 + ubyte.sizeof + long.sizeof]);
                auto labelAddress = module_.lookupAddress(label);
                Instructions.set!uint(labelAddress, &byteCode[address + 1 + ubyte.sizeof + long.sizeof]);
                address += ubyte.sizeof + long.sizeof + uint.sizeof;
            } else if (opcode == Opcodes.subrri) {
                address += ubyte.sizeof + ubyte.sizeof + long.sizeof;
            } else if (opcode == Opcodes.subrsi) {
                address += ubyte.sizeof + uint.sizeof + long.sizeof;
            } else if (opcode == Opcodes.addrri) {
                address += ubyte.sizeof + ubyte.sizeof + long.sizeof;
            } else if (opcode == Opcodes.loadri) {
                address += ubyte.sizeof + long.sizeof;
            } else if (opcode == Opcodes.pushr) {
                address += ubyte.sizeof;
            } else if (opcode == Opcodes.loadrs) {
                address += ubyte.sizeof + uint.sizeof;
            } else if (opcode == Opcodes.loadrr) {
                address += ubyte.sizeof + ubyte.sizeof;
            } else if (opcode == Opcodes.rcall) {
                auto label = Instructions.get!uint(&byteCode[address + 1]);
                auto labelAddress = module_.lookupAddress(label);
                Instructions.set!uint(labelAddress, &byteCode[address + 1]);
                address += uint.sizeof;
            } else if (opcode == Opcodes.jmp) {
                auto label = Instructions.get!uint(&byteCode[address + 1]);
                auto labelAddress = module_.lookupAddress(label);
                Instructions.set!uint(labelAddress, &byteCode[address + 1]);
                address += uint.sizeof;
            } else



            // Stack machine instructions
            if (opcode == Opcodes.push) {
                address += long.sizeof;
            } else if (opcode == Opcodes.pushs) {
                auto length = Instructions.get!ushort(&byteCode[address + 1]);
                address += ushort.sizeof + length;
            } else if (opcode == Opcodes.jump || opcode == Opcodes.cjump) {
                auto label = Instructions.get!uint(&byteCode[address + 1]);
                auto labelAddress = module_.lookupAddress(label);
                Instructions.set!uint(labelAddress, &byteCode[address + 1]);
                address += uint.sizeof;
            } else if (opcode == Opcodes.call) {
                auto label = Instructions.get!uint(&byteCode[address + 1]);
                auto labelAddress = module_.lookupAddress(label);
                Instructions.set!uint(labelAddress, &byteCode[address + 1]);
                address += uint.sizeof + ubyte.sizeof;
            } else if (opcode == Opcodes.ret) {
                address += ubyte.sizeof;
            } else if (opcode == Opcodes.spawn) {
                auto label = Instructions.get!uint(&byteCode[address + 1]);
                auto labelAddress = module_.lookupAddress(label);
                Instructions.set!uint(labelAddress, &byteCode[address + 1]);
                address += uint.sizeof + ubyte.sizeof;
            } else if (opcode == Opcodes.sys) {
                address += ushort.sizeof;
            }
            address++;
        }
    }

        private void assertOperands(ulong arity, ubyte expectedArity,
                                    string line) {
        if (arity != expectedArity) {
            throw new LoaderError("Invalid instruction '" ~ line ~ "'");
        }
    }

    private void assertOperands(ulong arity, ubyte[] expectedArities,
                                string line) {
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
        auto match = matchFirst(registerString, regex(`^r([0-9]+)`));
        if (match) {
            auto register = parse!ubyte(match.captures[1], line);
            if (register >= 0 && register <= Job.REGISTERS) {
                return register;
            }
        }
        throw new LoaderError("Invalid instruction '" ~ line ~ "'");
    }

    private long parseImmediateValue(string valueString, string line) {
        auto match = matchFirst(valueString, regex(`^#([0-9]+)`));
        if (match) {
            auto value = parse!long(match.captures[1], line);
            return value;
        }
        throw new LoaderError("Invalid instruction '" ~ line ~ "'");
    }

    private uint parseStackOffset(string stackOffsetString, string line) {
        auto match = matchFirst(stackOffsetString, regex(`^@([0-9]+)`));
        if (match) {
            auto stackOffset = parse!uint(match.captures[1], line);
            return stackOffset + 2;
        }
        throw new LoaderError("Invalid instruction '" ~ line ~ "'");
    }

    public bool isModuleLoaded(string moduleName) {
        return (moduleName in modules) != null;
    }

    public uint lookupAddress(string moduleName, uint label) {
        auto module_ = moduleName in modules;
        return module_.lookupAddress(label);
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
        module_.stopAddress = cast(uint)byteCode.length - 1;
        modules[moduleName] = module_;
    }

    public void prettyPrint() {
        uint address = 0;
        while (address < byteCode.length) {
            writef("%d: ", address);
            address += 1 + PrettyPrint.printInstruction(&byteCode[address]);
        }
    }

    public void prettyPrint(string moduleName) {
        auto module_ = modules[moduleName];
        auto address = module_.startAddress;
        while (address < module_.stopAddress + 1) {
            writef("%d: ", address);
            address += 1 + PrettyPrint.printInstruction(&byteCode[address]);
        }
    }
}

class Module {
    public uint startAddress;
    public uint stopAddress;
    private uint[uint] jumpTable;

    this(uint startAddress) {
        this.startAddress = startAddress;
    }

    public void insertLabel(uint label, uint address) {
        jumpTable[label] = address;
    }

    public uint lookupAddress(uint label) {
        return jumpTable[label];
    }

    public uint lookupLabel(uint address) {
        foreach (label, possibleAddress; jumpTable) {
            if (address == possibleAddress) {
                return label;
            }
        }
        throw new LoaderError("Internal Error");
    }
}
