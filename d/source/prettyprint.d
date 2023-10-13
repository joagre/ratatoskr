module prettyprint;

import std.stdio : File, writeln;
import std.conv : to;
import loader;

struct PrettyPrint {
    static public uint printInstruction(ubyte* bytes) {
        string getRegisterString(ubyte instruction) {
            auto register = instruction & 0b00000111;
            if (register == Registers.sp) {
                return "sp";
            } else { // Must be fp
                return "fp";
            }
        }

        switch (bytes[0] >> 3) {
        case Opcodes.push:
            auto value = Loader.get!long(&bytes[1]);
            writeln("push " ~ to!string(value));
            return long.sizeof;
        case Opcodes.pushs:
            auto length = Loader.get!ushort(&bytes[1]);
            auto index = 1 + ushort.sizeof;
            auto byteString = bytes[index .. index + length];
            writeln("pushs \"" ~ cast(string)byteString ~ "\"");
            return ushort.sizeof + length;
        case Opcodes.pop:
            writeln("pop");
            break;
        case Opcodes.dup:
            writeln("dup");
            break;
        case Opcodes.swap:
            writeln("swap");
            break;
        case Opcodes.load:
            string register = getRegisterString(bytes[0]);
            writeln("load " ~ register);
            break;
        case Opcodes.store:
            string register = getRegisterString(bytes[0]);
            writeln("store " ~ register);
            break;
        case Opcodes.add:
            writeln("add");
            break;
        case Opcodes.sub:
            writeln("sub");
            break;
        case Opcodes.mul:
            writeln("mul");
            break;
        case Opcodes.div:
            writeln("div");
            break;
        case Opcodes.jump:
            auto byteIndex = Loader.get!uint(&bytes[1]);
            writeln("jump " ~ to!string(byteIndex));
            return uint.sizeof;
        case Opcodes.cjump:
            auto byteIndex = Loader.get!uint(&bytes[1]);
            writeln("cjump " ~ to!string(byteIndex));
            return uint.sizeof;
        case Opcodes.call:
            auto byteIndex = Loader.get!uint(&bytes[1]);
            auto arity = Loader.get!ubyte(&bytes[1 + uint.sizeof]);
            writeln("call " ~ to!string(byteIndex) ~ " " ~ to!string(arity));
            return uint.sizeof + ubyte.sizeof;
        case Opcodes.mcall:
            auto byteIndex = Loader.get!uint(&bytes[1]);
            auto arity = Loader.get!ubyte(&bytes[1 + uint.sizeof]);
            writeln("call " ~ to!string(byteIndex) ~ " " ~ to!string(arity));
            return uint.sizeof + ubyte.sizeof;
        case Opcodes.ret:
            auto returnMode = bytes[0] & 0b00000111;
            if (returnMode == ReturnModes.copy) {
                writeln("ret copy");
            } else {
                writeln("ret");
            }
            break;
        case Opcodes.sys:
            auto value = Loader.get!uint(&bytes[1]);
            writeln("sys " ~ to!string(value));
            return uint.sizeof;
        case Opcodes.and:
            writeln("and");
            break;
        case Opcodes.or:
            writeln("or");
            break;
        case Opcodes.not:
            writeln("not");
            break;
        case Opcodes.eq:
            writeln("eq");
            break;
        case Opcodes.neq:
            writeln("neq");
            break;
        case Opcodes.lt:
            writeln("lt");
            break;
        case Opcodes.gt:
            writeln("gt");
            break;
        case Opcodes.nop:
            writeln("nop");
            break;
        case Opcodes.halt:
            writeln("halt");
            break;
        default:
            throw new LoaderError("Unknown opcode " ~
                                    to!string(bytes[0] >> 3));
        }

        return 0;
    }
}
