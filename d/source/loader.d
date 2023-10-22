module loader;

import std.stdio;
import std.string;
import std.conv;
import std.typecons;
import std.algorithm.searching;
import std.utf;
import std.regex;
import std.path;
import std.range;
import std.file;

import vm;
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
            string[] operands = null;
            if (firstBlank == -1) {
                opcodeString = line.strip;
            } else {
                opcodeString = line[0 .. firstBlank];
                operands = line[firstBlank + 1 .. $].split;
            }

            if (opcodeString == "label") {
                if (operands.length != 1) {
                    throw new LoaderError("Invalid instruction '" ~ line ~ "'");
                }
                module_.insertLabel(parse!LabelType(operands[0], line),
                                    cast(AddressType)byteCode.length);
                continue;
            }

            OpcodeInfo opcodeInfo;
            if (opcodeString in Vm.stringToOpcodeInfo) {
                opcodeInfo =
                    cast(OpcodeInfo)Vm.stringToOpcodeInfo[opcodeString];
                byteCode ~= opcodeInfo.opcode;
            } else {
                throw new LoaderError("Invalid instruction '" ~ line ~ "'");
            }

            // Insert opcode and its operand(s) into byte code
            auto operandBytes = getOperandsAsBytes(opcodeInfo, operands, line);
            byteCode ~= operandBytes;
        }

        // Resolve labels to addresses
        auto address = module_.startAddress;
        while (address < byteCode.length) {
            auto opcode = byteCode[address];
            auto operandAddress = address + cast(AddressType)Opcode.sizeof;
            // Register machine instructions
            if (opcode == Opcode.jmprnze) {
                resolveLabel(byteCode, module_, operandAddress,
                             RegisterType.sizeof);
                address += sizeOfOperands(Opcode.jmprnze);
            } else if (opcode == Opcode.jmpringt) {
                resolveLabel(byteCode, module_, operandAddress,
                             RegisterType.sizeof + ImmediateValueType.sizeof);
                address += sizeOfOperands(Opcode.jmpringt);
            } else if (opcode == Opcode.rcall) {
                resolveLabel(byteCode, module_, operandAddress, 0);
                address += sizeOfOperands(Opcode.rcall);
            } else if (opcode == Opcode.jmp) {
                resolveLabel(byteCode, module_, operandAddress, 0);
                address += sizeOfOperands(Opcode.jmp);
            // Stack machine instructions
            } else if (opcode == Opcode.pushs) {
                auto length =
                    Vm.getValue!DataLengthType(&byteCode[operandAddress]);
                address += DataLengthType.sizeof + length;
            } else if (opcode == Opcode.jump) {
                resolveLabel(byteCode, module_, operandAddress, 0);
                address += sizeOfOperands(Opcode.jump);
            } else if (opcode == Opcode.cjump) {
                resolveLabel(byteCode, module_, operandAddress, 0);
                address += sizeOfOperands(Opcode.cjump);
            } else if (opcode == Opcode.call) {
                resolveLabel(byteCode, module_, operandAddress, 0);
                address += sizeOfOperands(Opcode.call);
            } else if (opcode == Opcode.spawn) {
                resolveLabel(byteCode, module_, operandAddress, 0);
                address += sizeOfOperands(Opcode.spawn);
            } else {
                address += sizeOfOperands(cast(Opcode)opcode);
            }
            address++;
        }
    }

    private void resolveLabel(ubyte[] byteCode, Module module_,
                              AddressType firstOperand, uint operandOffset) {
        auto labelAddress = firstOperand + operandOffset;
        auto label = Vm.getValue!LabelType(&byteCode[labelAddress]);
        auto address = module_.lookupAddress(label);
        Vm.setValue!AddressType(address, &byteCode[labelAddress]);
    }

    public bool isModuleLoaded(string moduleName) {
        return (moduleName in modules) != null;
    }

    public AddressType lookupAddress(string moduleName, LabelType label) {
        auto module_ = moduleName in modules;
        return module_.lookupAddress(label);
    }

    public void loadModule(string moduleName) {
        auto filename = buildPath(loadPath, moduleName ~ ".posm");
        if (!exists(filename)) {
            throw new LoaderError(filename ~  " can not be found");
        }
        file = File(filename, "r");
        Module module_ = new Module(cast(AddressType)byteCode.length);
        try {
            generateByteCode(module_);
        } finally {
            file.close;
        }
        module_.stopAddress = cast(AddressType)byteCode.length - 1;
        modules[moduleName] = module_;
    }

    public void prettyPrint() {
        AddressType address = 0;
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

    private ubyte[] getOperandsAsBytes(ref OpcodeInfo opcodeInfo,
                                       string[] operands, string line) {
        if (!(opcodeInfo.opcode == Opcode.pushs ||
              opcodeInfo.opcode == Opcode.ret ||
              operands.length == opcodeInfo.operandTypes.length)) {
            throw new VmError("Wrong number of operands in '" ~ line ~ "'");
        }

        ubyte[] bytes;
        for (auto i = 0; i < opcodeInfo.operandTypes.length; i++) {
            final switch(opcodeInfo.operandTypes[i]) {
            case OperandType.stackValue:
                auto stackValue = parse!StackValueType(operands[i], line);
                insertBytes(stackValue, bytes);
                break;
            case OperandType.register:
                auto match = matchFirst(operands[i], regex(`^r([0-9]+)`));
                if (match) {
                    auto register =
                        parse!RegisterType(match.captures[1], line);
                    if (register >= 0 && register <= Vm.numberOfRegisters) {
                        insertBytes(register, bytes);
                    } else {
                        throw new VmError("Bad register in '" ~ line ~ "'");
                    }
                } else {
                    throw new VmError("Bad register in '" ~ line ~ "'");
                }
                break;
            case OperandType.label:
                auto label = parse!LabelType(operands[i], line);
                insertBytes(label, bytes);
                break;
            case OperandType.immediateValue:
                auto match = matchFirst(operands[i], regex(`^#([0-9]+)`));
                if (match) {
                    auto immediateValue =
                        parse!ImmediateValueType(match.captures[1], line);
                    insertBytes(immediateValue, bytes);
                } else {
                    throw new VmError("Bad immediate value in '" ~ line ~ "'");
                }
                break;
            case OperandType.stackOffset:
                auto match = matchFirst(operands[i], regex(`^@([0-9]+)`));
                if (match) {
                    auto stackOffset =
                        parse!StackOffsetType(match.captures[1], line);
                    // FP points at the return address (followed by the
                    // previous FP)
                    insertBytes(stackOffset + 2, bytes);
                } else {
                    throw new VmError("Bad stack offset in '" ~ line ~ "'");
                }
                break;
            case OperandType.arity:
                auto arity = parse!ArityType(operands[i], line);
                insertBytes(arity, bytes);
                break;
            case OperandType.returnMode:
                if (operands.length == 0) {
                    insertBytes(ReturnMode.value, bytes);
                } else if (operands[0] == "copy") {
                    insertBytes(ReturnMode.copy, bytes);
                } else {
                    throw new VmError("Invalid return mode in '" ~ line ~ "'");
                }
                break;
            case OperandType.systemCall:
                if (operands[0] in Vm.stringToSystemCall) {
                    SystemCall systemCall = Vm.stringToSystemCall[operands[0]];
                    insertBytes(systemCall, bytes);
                } else {
                    throw new VmError("Invalid system call in '" ~ line ~ "'");
                }
                break;
            case OperandType.string:
                ubyte[] stringBytes =
                    cast(ubyte[])toUTF8(operands.join(" ").strip(`"`));
                insertBytes(cast(DataLengthType)stringBytes.length, bytes);
                bytes ~= stringBytes;
                break;
            }
        }

        return bytes;
    }

    private uint sizeOfOperands(Opcode opcode) {
        auto opcodeInfo = Vm.stringToOpcodeInfo[to!string(opcode)];
        uint size = 0;
        foreach(operandType; opcodeInfo.operandTypes) {
            final switch(operandType) {
            case OperandType.stackValue:
                size += StackValueType.sizeof;
                break;
            case OperandType.register:
                size += RegisterType.sizeof;
                break;
            case OperandType.label:
                size += LabelType.sizeof;
                break;
            case OperandType.immediateValue:
                size += ImmediateValueType.sizeof;
                break;
            case OperandType.stackOffset:
                size += StackOffsetType.sizeof;
                break;
            case OperandType.arity:
                size += ArityType.sizeof;
                break;
            case OperandType.returnMode:
                size += ReturnModeType.sizeof;
                break;
            case OperandType.systemCall:
                size += SystemCallType.sizeof;
                break;
            case OperandType.string:
                // String length calculation is taken care of elsewhere
            }
        }
        return size;
    }

    private T parse(T)(string value, string line)
         if(is(T == StackValueType) ||
            is(T == RegisterType) ||
            is(T == ImmediateValueType) ||
            is(T == StackOffsetType) ||
            is(T == DataLengthType) ||
            is(T == ArityType) ||
            is(T == ReturnModeType) ||
            is(T == SystemCallType)) {
             try {
                 return to!T(value);
             } catch (ConvException) {
                 throw new VmError("Invalid operands in '" ~ line ~ "'");
             }
         }

    private void insertBytes(T)(T value, ref ubyte[] bytes) {
        bytes ~= (cast(ubyte*)&value)[0 .. T.sizeof];
    }
}

class Module {
    public AddressType startAddress;
    public AddressType stopAddress;
    private AddressType[LabelType] jumpTable;

    this(AddressType startAddress) {
        this.startAddress = startAddress;
    }

    public void insertLabel(LabelType label, AddressType address) {
        jumpTable[label] = address;
    }

    public AddressType lookupAddress(LabelType label) {
        return jumpTable[label];
    }

    public LabelType lookupLabel(AddressType address) {
        foreach (label, possibleAddress; jumpTable) {
            if (address == possibleAddress) {
                return label;
            }
        }
        throw new LoaderError("Internal Error");
    }
}
