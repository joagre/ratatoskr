module runcontext;

import std.stdio : writeln;
import std.conv : to;
import std.typecons : Tuple;
import program;

struct RunContext {
    public long rcid;
    public long pc = 0; // FIXME: Do not set to zero, set in constructor
    public CallStack callStack;
    public DataStack dataStack;

    // Remove
    public Program* program;

    // FIXME: rework
    this(long rcid, Program* program) {
        this.rcid = rcid;
        this.program = program;
    }

    public string popString() {
        auto dataAddress = callStack.pop();
        auto bytes = dataStack.peek(dataAddress);
        return cast(string)bytes[4 .. $];
    }
}

struct CallStack  {
    public long[] stack;
    public long fp = 0;

    public long length() {
        return stack.length;
    }

    public void set(long[] newStack) {
        stack = newStack;
    }

    public void append(long[] newStack) {
        stack ~= newStack;
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
}

struct DataStack  {
    public ubyte[] stack;
    public long fp = 0;

    public long length() {
        return stack.length;
    }

    public Tuple!(long, int) push(ubyte[] bytes) {
        auto length = get!int(&bytes[0]);
        auto dataAddress = stack.length;
        stack ~= bytes[0 .. 4 + length];
        return Tuple!(long, int)(dataAddress, length);
    }

    public ubyte[] peek(long dataAddress) {
        ubyte[] bytes = stack[dataAddress .. $];
        auto length = get!int(&bytes[0]);
        return bytes[0 .. 4 + length];
    }
}
