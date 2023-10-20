module prettyprint;

import std.stdio : File, writeln, writefln;
import std.conv : to;

import loader;
import instructions;

class PrettyPrint {
    static public uint printInstruction(ubyte* bytes) {
        switch (bytes[0]) {
        // Register machine instructions
        case Opcodes.jmprnze:
            auto register = Instructions.get!ubyte(&bytes[1]);
            auto address = Instructions.get!uint(&bytes[1 + ubyte.sizeof]);
            writefln("jmprnze r%d %d", register, address);
            return ubyte.sizeof + uint.sizeof;
        case Opcodes.jmpringt:
            /*
            '(register, immediateValue, address, size) =
               ril(job.pc, byte_code);
            writefln("jmpringt r%d #%d %d", register, value, address);
            return size;
            */


            auto register = Instructions.get!ubyte(&bytes[1]);
            auto value = Instructions.get!long(&bytes[1 + ubyte.sizeof]);
            auto address =
                Instructions.get!uint(&bytes[1 + ubyte.sizeof + long.sizeof]);
            writefln("jmpringt r%d #%d %d", register, value, address);
            return ubyte.sizeof + long.sizeof + uint.sizeof;
        case Opcodes.subrri:
            auto firstRegister = Instructions.get!ubyte(&bytes[1]);
            auto secondRegister =
                Instructions.get!ubyte(&bytes[1 + ubyte.sizeof]);
            auto value =
                Instructions.get!long(&bytes[1 + ubyte.sizeof + ubyte.sizeof]);
            writefln("subrri r%d r%d #%d", firstRegister, secondRegister,
                     value);
            return ubyte.sizeof + ubyte.sizeof + long.sizeof;
        case Opcodes.subrsi:
            auto register = Instructions.get!ubyte(&bytes[1]);
            auto stackOffset = Instructions.get!uint(&bytes[1 + ubyte.sizeof]);
            auto value = Instructions.get!long(&bytes[1 + ubyte.sizeof + uint.sizeof]);
            writefln("subrsi r%d @%d #%d", register, stackOffset, value);
            return ubyte.sizeof + uint.sizeof + long.sizeof;
        case Opcodes.addrri:
            auto firstRegister = Instructions.get!ubyte(&bytes[1]);
            auto secondRegister =
                Instructions.get!ubyte(&bytes[1 + ubyte.sizeof]);
            auto value =
                Instructions.get!long(&bytes[1 + ubyte.sizeof + ubyte.sizeof]);
            writefln("addrri r%d r%d #%d", firstRegister, secondRegister,
                     value);
            return ubyte.sizeof + ubyte.sizeof + long.sizeof;
        case Opcodes.loadri:
            auto register = Instructions.get!ubyte(&bytes[1]);
            auto value = Instructions.get!long(&bytes[1 + ubyte.sizeof]);
            writefln("loadri r%d #%d", register, value);
            return ubyte.sizeof + long.sizeof;
        case Opcodes.pushr:
            auto register = Instructions.get!ubyte(&bytes[1]);
            writefln("pushr r%d", register);
            return ubyte.sizeof;
        case Opcodes.loadrs:
            auto register = Instructions.get!ubyte(&bytes[1]);
            auto stackOffset = Instructions.get!uint(&bytes[1 + ubyte.sizeof]);
            writefln("loadrs r%d @%d", register, stackOffset);
            return ubyte.sizeof + uint.sizeof;
        case Opcodes.loadrr:
            auto firstRegister = Instructions.get!ubyte(&bytes[1]);
            auto secondRegister =
                Instructions.get!ubyte(&bytes[1 + ubyte.sizeof]);
            writefln("loadrr r%d r%d", firstRegister, secondRegister);
            return ubyte.sizeof + ubyte.sizeof;





        case Opcodes.rcall:
            auto address = Instructions.get!uint(&bytes[1]);
            writefln("rcall %d", address);
            return uint.sizeof;
        case Opcodes.jmp:
            auto address = Instructions.get!uint(&bytes[1]);
            writefln("jmp %d", address);
            return uint.sizeof;
        // Stack machine instructions
        case Opcodes.push:
            auto value = Instructions.get!long(&bytes[1]);
            writefln("push %d", value);
            return long.sizeof;
        case Opcodes.pushs:
            auto length = Instructions.get!ushort(&bytes[1]);
            auto index = 1 + ushort.sizeof;
            auto byteString = bytes[index .. index + length];
            writefln("pushs \"%s\"", cast(string)byteString);
            return ushort.sizeof + length;
        case Opcodes.jump:
            auto address = Instructions.get!uint(&bytes[1]);
            writefln("jump %d", address);
            return uint.sizeof;
        case Opcodes.cjump:
            auto address = Instructions.get!uint(&bytes[1]);
            writefln("cjump %d", address);
            return uint.sizeof;
        case Opcodes.call:
            auto address = Instructions.get!uint(&bytes[1]);
            auto arity = Instructions.get!ubyte(&bytes[1 + uint.sizeof]);
            writefln("call %d %d", address, arity);
            return uint.sizeof + ubyte.sizeof;
        case Opcodes.ret:
            auto returnMode = Instructions.get!ubyte(&bytes[1]);
            if (returnMode == ReturnModes.copy) {
                writeln("ret copy");
            } else {
                writeln("ret");
            }
            return ubyte.sizeof;
        case Opcodes.sys:
            auto name = Instructions.get!ushort(&bytes[1]);
            writefln("sys %d",  name);
            return ushort.sizeof;
        case Opcodes.spawn:
            auto address = Instructions.get!uint(&bytes[1]);
            auto arity = Instructions.get!ubyte(&bytes[1 + uint.sizeof]);
            writefln("spawn %d %d", address, arity);
            return uint.sizeof + ubyte.sizeof;
        default:
            writeln(Instructions.opcodeToString[to!Opcodes(bytes[0])]);
        }

        return 0;
    }
}
