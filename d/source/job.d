module job;

import std.stdio : writeln;
import std.conv : to;
import std.typecons : Tuple;

import loader;

class Job {
    public uint jid;
    public uint pc;
    public CallStack callStack;
    public DataStack dataStack;

    this(uint jid, uint pc) {
        this.jid = jid;
        this.pc = pc;
        this.callStack = new CallStack;
        this.dataStack = new DataStack;
    }

    public string popString() {
        auto dataAddress = callStack.pop();
        auto bytes = dataStack.peek(dataAddress);
        return cast(string)bytes[ushort.sizeof .. $];
    }
}

class CallStack  {
    public long[] stack;
    public long fp = -1;

    public long length() {
        return stack.length;
    }

    public void append(long[] trailingStack) {
        stack ~= trailingStack;
    }

    //    public void set(long[] newStack) {
    //    stack = newStack;
    // }

    public void push(long value) {
        stack ~= value;
    }

    public long pop() {
        auto topValue = stack[$ - 1];
        stack = stack[0 .. $ - 1];
        return topValue;
    }

    public void dup() {
        auto topValue = stack[$ - 1];
        stack ~= topValue;
    }

    public void swap() {
        auto topValue = stack[$ - 1];
        stack[$ - 1] = stack[$ - 2];
        stack[$ - 2] = topValue;
    }

    public void load(ubyte register) {
        auto offset = pop();
        if (register == Registers.sp) {
            push(stack[$ - 1 - offset]);
        } else { // Must be fp
            push(stack[fp - offset]);
        }
    }

    public void store(ubyte register) {
        auto offset = pop();
        auto newValue = pop();
        if (register == Registers.sp) {
            stack[$ - 1 - offset] = newValue;
        } else { // Must be fp
            stack[fp - offset] = newValue;
        }
    }

    public void op(long delegate(long, long) fun) {
        auto operand2 = pop();
        auto operand1 = pop();
        push(fun(operand1, operand2));
    }
}

class DataStack  {
    public ubyte[] stack;
    public long fp = 0;

    public long length() {
        return stack.length;
    }

    public Tuple!(long, ushort) push(ubyte[] bytes) {
        auto length = Loader.get!ushort(&bytes[0]);
        long dataAddress = stack.length;
        stack ~= bytes[0 .. ushort.sizeof + length];
        return Tuple!(long, ushort)(dataAddress, length);
    }

    public ubyte[] peek(long dataAddress) {
        ubyte[] bytes = stack[dataAddress .. $];
        auto length = Loader.get!ushort(&bytes[0]);
        return bytes[0 .. ushort.sizeof + length];
    }
}
