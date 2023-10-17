module prettyprint;

import std.stdio : File, writeln, writefln;
import std.conv : to;

import loader;

class PrettyPrint {
    static public uint printInstruction(ubyte* bytes) {
        switch (bytes[0] >> OPCODE_BITS) {
        case Opcodes.push:
            auto value = Loader.get!long(&bytes[1]);
            writefln("push %d", value);
            return long.sizeof;
        case Opcodes.pushs:
            auto length = Loader.get!ushort(&bytes[1]);
            auto index = 1 + ushort.sizeof;
            auto byteString = bytes[index .. index + length];
            writefln("pushs \"%s\"", cast(string)byteString);
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
            writeln("load");
            break;
        case Opcodes.store:
            writeln("store");
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
            writefln("jump %d", byteIndex);
            return uint.sizeof;
        case Opcodes.cjump:
            auto byteIndex = Loader.get!uint(&bytes[1]);
            writefln("cjump %d", byteIndex);
            return uint.sizeof;
        case Opcodes.call:
            auto byteIndex = Loader.get!uint(&bytes[1]);
            auto arity = Loader.get!ubyte(&bytes[1 + uint.sizeof]);
            writefln("call %d %d", byteIndex, arity);
            return uint.sizeof + ubyte.sizeof;
        case Opcodes.mcall:
            writeln("mcall");
            break;
        case Opcodes.ret:
            auto returnMode = bytes[0] & OPCODE_OPERAND_MASK;
            if (returnMode == ReturnModes.copy) {
                writeln("ret copy");
            } else {
                writeln("ret");
            }
            break;
        case Opcodes.sys:
            auto value = Loader.get!uint(&bytes[1]);
            writefln("sys %d",  value);
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
        case Opcodes.spawn:
            auto byteIndex = Loader.get!uint(&bytes[1]);
            auto arity = Loader.get!ubyte(&bytes[1 + uint.sizeof]);
            writefln("spawn %d %d", byteIndex, arity);
            return uint.sizeof + ubyte.sizeof;
        case Opcodes.mspawn:
            writeln("mspawn");
            break;
        default:
            throw new LoaderError("Unknown opcode " ~
                                  to!string(bytes[0] >> OPCODE_BITS));
        }

        return 0;
    }
}
