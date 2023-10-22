module prettyprint;

import std.stdio;
import std.conv;

import vm;

class PrettyPrint {
    static public uint printInstruction(ubyte* bytes) {
        ubyte* ops = bytes + Opcode.sizeof;

        switch (*bytes) {
        // Register machine instructions
        case Opcode.jmprnze:
            uint size = 0;
            auto register = mixin(Operand!(RegisterType, "ops", "size"));
            auto address = mixin(Operand!(AddressType, "ops", "size"));
            writefln("jmprnze r%d %d", register, address);
            return size;
        case Opcode.jmpringt:
            uint size = 0;
            auto register = mixin(Operand!(RegisterType, "ops", "size"));
            auto value = mixin(Operand!(ImmediateValueType, "ops", "size"));
            auto address = mixin(Operand!(AddressType, "ops", "size"));
            writefln("jmpringt r%d #%d %d", register, value, address);
            return size;
        case Opcode.subrri:
            uint size = 0;
            auto firstRegister = mixin(Operand!(RegisterType, "ops", "size"));
            auto secondRegister = mixin(Operand!(RegisterType, "ops", "size"));
            auto value = mixin(Operand!(ImmediateValueType, "ops", "size"));
            writefln("subrri r%d r%d #%d", firstRegister, secondRegister,
                     value);
            return size;
        case Opcode.subrsi:
            uint size = 0;
            auto register = mixin(Operand!(RegisterType, "ops", "size"));
            auto stackOffset = mixin(Operand!(StackOffsetType, "ops", "size"));
            auto value = mixin(Operand!(ImmediateValueType, "ops", "size"));
            writefln("subrsi r%d @%d #%d", register, stackOffset, value);
            return size;
        case Opcode.addrri:
            uint size = 0;
            auto firstRegister = mixin(Operand!(RegisterType, "ops", "size"));
            auto secondRegister = mixin(Operand!(RegisterType, "ops", "size"));
            auto value = mixin(Operand!(ImmediateValueType, "ops", "size"));
            writefln("addrri r%d r%d #%d", firstRegister, secondRegister,
                     value);
            return size;
        case Opcode.loadri:
            uint size = 0;
            auto register = mixin(Operand!(RegisterType, "ops", "size"));
            auto value = mixin(Operand!(ImmediateValueType, "ops", "size"));
            writefln("loadri r%d #%d", register, value);
            return size;
        case Opcode.pushr:
            uint size = 0;
            auto register = mixin(Operand!(RegisterType, "ops", "size"));
            writefln("pushr r%d", register);
            return size;
        case Opcode.loadrs:
            uint size = 0;
            auto register = mixin(Operand!(RegisterType, "ops", "size"));
            auto stackOffset = mixin(Operand!(StackOffsetType, "ops", "size"));
            writefln("loadrs r%d @%d", register, stackOffset);
            return size;
        case Opcode.loadrr:
            uint size = 0;
            auto firstRegister = mixin(Operand!(RegisterType, "ops", "size"));
            auto secondRegister = mixin(Operand!(RegisterType, "ops", "size"));
            writefln("loadrr r%d r%d", firstRegister, secondRegister);
            return size;
        case Opcode.rcall:
            uint size = 0;
            auto address = mixin(Operand!(AddressType, "ops", "size"));
            writefln("rcall %d", address);
            return size;
        case Opcode.jmp:
            uint size = 0;
            auto address = mixin(Operand!(AddressType, "ops", "size"));
            writefln("jmp %d", address);
            return size;
        // Stack machine instructions
        case Opcode.push:
            uint size = 0;
            auto stackValue = mixin(Operand!(StackValueType, "ops", "size"));
            writefln("push %d", stackValue);
            return size;
        case Opcode.pushs:
            uint size = 0;
            auto length = mixin(Operand!(DataLengthType, "ops", "size"));
            auto index = ubyte.sizeof + size;
            auto byteString = bytes[index .. index + length];
            writefln("pushs \"%s\"", cast(string)byteString);
            return size + length;
        case Opcode.jump:
            uint size = 0;
            auto address = mixin(Operand!(AddressType, "ops", "size"));
            writefln("jump %d", address);
            return size;
        case Opcode.cjump:
            uint size = 0;
            auto address = mixin(Operand!(AddressType, "ops", "size"));
            writefln("cjump %d", address);
            return size;
        case Opcode.call:
            uint size = 0;
            auto address = mixin(Operand!(AddressType, "ops", "size"));
            auto arity = mixin(Operand!(ArityType, "ops", "size"));
            writefln("call %d %d", address, arity);
            return size;
        case Opcode.ret:
            uint size = 0;
            auto returnMode = mixin(Operand!(ReturnModeType, "ops", "size"));
            if (returnMode == ReturnMode.copy) {
                writeln("ret copy");
            } else {
                writeln("ret");
            }
            return size;
        case Opcode.sys:
            uint size = 0;
            auto systemCall = mixin(Operand!(SystemCallType, "ops", "size"));
            writefln("sys %d",  systemCall);
            return size;
        case Opcode.spawn:
            uint size = 0;
            auto address = mixin(Operand!(AddressType, "ops", "size"));
            auto arity = mixin(Operand!(ArityType, "ops", "size"));
            writefln("spawn %d %d", address, arity);
            return size;
        default:
            writeln(Vm.opcodeToString[to!Opcode(bytes[0])]);
        }

        return 0;
    }
}
