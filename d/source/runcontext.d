module runcontext;

import std.stdio : writeln;
import std.conv : to;
import std.typecons : Tuple;
import program;

struct RunContext {
    public long rcid;

    public Program* program;
    // Program Counter
    public long pc = 0;

    // Call stack
    public long[] stack;
    // Frame pointer
    public long fp = 0;

    // Data stack
    public ubyte[] dataStack;
    // Data Frame Pointer
    public long dataFp = 0;

    this(long rcid, Program* program) {
        this.rcid = rcid;
        this.program = program;
    }

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

    public Tuple!(long, int) pushData(ubyte[] bytes) {
        auto length = get!int(&bytes[0]);
        auto dataAddress = dataStack.length;
        dataStack ~= bytes[0 .. 4 + length];
        return Tuple!(long, int)(dataAddress, length);
    }

    public ubyte[] peekData(long dataAddress) {
        ubyte[] bytes = dataStack[dataAddress .. $];
        auto length = get!int(&bytes[0]);
        return bytes[0 .. 4 + length];
    }

    public string popString() {
        auto dataAddress = pop();
        auto bytes = peekData(dataAddress);
        return cast(string)bytes[4 .. $];
    }

    public void setStack(long[] newStack) {
        stack = newStack;
    }

    public void appendStack(long[] newStack) {
        stack ~= newStack;
    }
}
