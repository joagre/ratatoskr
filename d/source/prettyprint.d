module prettyprint;

import std.stdio : File, writeln, writefln;
import std.conv : to;

import loader;

class PrettyPrint {
    static public uint printInstruction(ubyte* bytes) {
        switch (bytes[0]) {
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
        case Opcodes.ret:
            auto returnMode = Loader.get!ubyte(&bytes[1]);
            if (returnMode == ReturnModes.copy) {
                writeln("ret copy");
            } else {
                writeln("ret");
            }
            return ubyte.sizeof;
        case Opcodes.sys:
            auto name = Loader.get!ushort(&bytes[1]);
            writefln("sys %d",  name);
            return ushort.sizeof;
        case Opcodes.spawn:
            auto byteIndex = Loader.get!uint(&bytes[1]);
            auto arity = Loader.get!ubyte(&bytes[1 + uint.sizeof]);
            writefln("spawn %d %d", byteIndex, arity);
            return uint.sizeof + ubyte.sizeof;
        default:
            writeln(Loader.opcodeToString[to!Opcodes(bytes[0])]);
        }

        return 0;
    }
}
