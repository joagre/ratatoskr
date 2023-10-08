module fiber;

import std.typecons : Tuple;
import program;

struct Fiber {
    public long fid;
    public Program* program;
    // Program Counter
    public long PC = 0;

    // Call stack
    public long[] stack;
    // Frame pointer
    public long FP = 0;

    // Data stack
    public ubyte[] dataStack;
    // Data Frame Pointer
    public long DATA_FP = 0;

    this(long fid, Program* program) {
        this.fid = fid;
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
        if (register == Registers.SP) {
            push(stack[$ - 1 - offset]);
        } else { // Must be FP
            push(stack[FP - offset]);
        }
    }

    public void store(ubyte register) {
        auto offset = pop();
        auto newValue = pop();
        if (register == Registers.SP) {
            stack[$ - 1 - offset] = newValue;
        } else { // Must be FP
            stack[FP - offset] = newValue;
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
        dataStack ~= bytes[0 .. 4 + length + 1];
        return Tuple!(long, int)(dataAddress, length);
    }

    public ubyte[] peekData(long dataAddress) {
        ubyte[] bytes = dataStack[dataAddress .. $];
        auto length = get!int(&bytes[0]);
        return bytes[0 .. 4 + length + 1];
    }

    public string popString() {
        long dataAddress = pop();
        auto bytes = peekData(dataAddress);
        return cast(string)bytes[4 .. $];
    }
}
