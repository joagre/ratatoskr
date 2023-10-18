module prettyprint;

import std.stdio : File, writeln, writefln;
import std.conv : to;

import loader;
import instructions;

class PrettyPrint {
    static public uint printInstruction(ubyte* bytes) {
        switch (bytes[0]) {
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
            auto byteIndex = Instructions.get!uint(&bytes[1]);
            writefln("jump %d", byteIndex);
            return uint.sizeof;
        case Opcodes.cjump:
            auto byteIndex = Instructions.get!uint(&bytes[1]);
            writefln("cjump %d", byteIndex);
            return uint.sizeof;
        case Opcodes.call:
            auto byteIndex = Instructions.get!uint(&bytes[1]);
            auto arity = Instructions.get!ubyte(&bytes[1 + uint.sizeof]);
            writefln("call %d %d", byteIndex, arity);
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
            auto byteIndex = Instructions.get!uint(&bytes[1]);
            auto arity = Instructions.get!ubyte(&bytes[1 + uint.sizeof]);
            writefln("spawn %d %d", byteIndex, arity);
            return uint.sizeof + ubyte.sizeof;
        default:
            writeln(Instructions.opcodeToString[to!Opcodes(bytes[0])]);
        }

        return 0;
    }
}
