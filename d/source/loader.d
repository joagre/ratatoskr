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
                    throw new LoaderError("Invalid instruction '" ~ line ~
                                          "'");
                }
                module_.insertLabel(Vm.parse!AddressType(operands[0], line),
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
            auto operandBytes = Vm.getOperandsAsBytes(opcodeInfo, operands,
                                                      line);
            byteCode ~= operandBytes;
        }

        // Resolve labels to addresses
        uint address = module_.startAddress;
        while (address < byteCode.length) {
            auto opcode = byteCode[address];
            uint operandAddress = address + cast(uint)Opcode.sizeof;
            // Register machine instructions
            if (opcode == Opcode.jmprnze) {
                resolveLabel(byteCode, module_, operandAddress,
                             RegisterType.sizeof);
                address += Vm.sizeOfOperands(Opcode.jmprnze);
            } else if (opcode == Opcode.jmpringt) {
                resolveLabel(byteCode, module_, operandAddress,
                             RegisterType.sizeof + ImmediateValueType.sizeof);
                address += Vm.sizeOfOperands(Opcode.jmpringt);
            } else if (opcode == Opcode.rcall) {
                resolveLabel(byteCode, module_, operandAddress, 0);
                address += Vm.sizeOfOperands(Opcode.rcall);
            } else if (opcode == Opcode.jmp) {
                resolveLabel(byteCode, module_, operandAddress, 0);
                address += Vm.sizeOfOperands(Opcode.jmp);
            // Stack machine instructions
            } else if (opcode == Opcode.pushs) {
                auto length =
                    Vm.getValue!DataLengthType(&byteCode[operandAddress]);
                address += DataLengthType.sizeof + length;
            } else if (opcode == Opcode.jump) {
                resolveLabel(byteCode, module_, operandAddress, 0);
                address += Vm.sizeOfOperands(Opcode.jump);
            } else if (opcode == Opcode.cjump) {
                resolveLabel(byteCode, module_, operandAddress, 0);
                address += Vm.sizeOfOperands(Opcode.cjump);
            } else if (opcode == Opcode.call) {
                resolveLabel(byteCode, module_, operandAddress, 0);
                address += Vm.sizeOfOperands(Opcode.call);
            } else if (opcode == Opcode.spawn) {
                resolveLabel(byteCode, module_, operandAddress, 0);
                address += Vm.sizeOfOperands(Opcode.spawn);
            } else {
                address += Vm.sizeOfOperands(cast(Opcode)opcode);
            }
            address++;
        }
    }

    private void resolveLabel(ubyte[] byteCode, Module module_,
                              uint firstOperand, uint operandOffset) {
        auto labelAddress = firstOperand + operandOffset;
        auto label = Vm.getValue!AddressType(&byteCode[labelAddress]);
        auto address = module_.lookupAddress(label);
        Vm.setValue!AddressType(address, &byteCode[labelAddress]);
    }

    private T parse(T)(string value, string line)
         if (is(T == byte) || is(T == ubyte) || is(T == short) ||
             is(T == ushort) || is(T == int) || is(T == uint) ||
             is(T == long) || is(T == ulong)) {
             try {
                 return to!T(value);
             } catch (ConvException) {
                 throw new LoaderError("Invalid operands in '" ~ line ~ "'");
             }
         }

    public bool isModuleLoaded(string moduleName) {
        return (moduleName in modules) != null;
    }

    public uint lookupAddress(string moduleName, uint label) {
        auto module_ = moduleName in modules;
        return module_.lookupAddress(label);
    }

    public void loadModule(string moduleName) {
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
