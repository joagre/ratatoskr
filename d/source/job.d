module job;

import std.stdio : writeln;
import std.conv : to;
import std.typecons : Tuple;
import std.container : DList;

import loader;

enum JobMode : ubyte {
    init,
    running,
    ready,
    waiting
}

class Job {
    public uint jid;
    public JobMode mode;
    public uint pc;
    public DataStack dataStack;
    public CallStack callStack;
    public MessageBox messageBox;

    this(uint jid, uint pc) {
        this.jid = jid;
        this.mode = JobMode.init;
        this.pc = pc;
        this.dataStack = new DataStack;
        this.callStack = new CallStack(this.dataStack);
        this.messageBox = new MessageBox;
    }
}

class CallStack  {
    public long[] stack;
    public long fp;
    public DataStack dataStack;

    this(DataStack dataStack) {
        this.fp = -1;
        this.dataStack = dataStack;
    }

    pragma(inline, true)
    public long length() {
        return stack.length;
    }

    pragma(inline, true)
    public void append(long[] trailingStack) {
        stack ~= trailingStack;
    }

    pragma(inline, true)
    public void push(long value) {
        stack ~= value;
    }

    pragma(inline, true)
    public string popString() {
        auto dataAddress = pop();
        auto bytes = dataStack.peek(dataAddress);
        return cast(string)bytes[ushort.sizeof .. $];
    }

    pragma(inline, true)
    public long pop() {
        auto topValue = stack[$ - 1];
        stack = stack[0 .. $ - 1];
        return topValue;
    }

    pragma(inline, true)
    public void dup() {
        auto topValue = stack[$ - 1];
        stack ~= topValue;
    }

    pragma(inline, true)
    public void swap() {
        auto topValue = stack[$ - 1];
        stack[$ - 1] = stack[$ - 2];
        stack[$ - 2] = topValue;
    }

    pragma(inline, true)
    public void load(ubyte register) {
        auto offset = pop();
        if (register == Registers.sp) {
            push(stack[$ - 1 - offset]);
        } else { // Must be fp
            push(stack[fp - offset]);
        }
    }

    pragma(inline, true)
    public void store(ubyte register) {
        auto offset = pop();
        auto newValue = pop();
        if (register == Registers.sp) {
            stack[$ - 1 - offset] = newValue;
        } else { // Must be fp
            stack[fp - offset] = newValue;
        }
    }

    pragma(inline, true)
    public void op(long delegate(long, long) fun) {
        auto operand2 = pop();
        auto operand1 = pop();
        push(fun(operand1, operand2));
    }
}

class DataStack  {
    public ubyte[] stack;
    public long fp;

    this() {
        this.fp = 0;
    }

    pragma(inline, true)
    public long length() {
        return stack.length;
    }

    pragma(inline, true)
    public Tuple!(long, ushort) push(ubyte[] bytes) {
        auto length = Loader.get!ushort(&bytes[0]);
        long dataAddress = stack.length;
        stack ~= bytes[0 .. ushort.sizeof + length];
        return Tuple!(long, ushort)(dataAddress, length);
    }

    pragma(inline, true)
    public ubyte[] peek(long dataAddress) {
        ubyte[] bytes = stack[dataAddress .. $];
        auto length = Loader.get!ushort(&bytes[0]);
        return bytes[0 .. ushort.sizeof + length];
    }
}

class MessageBox {
    private DList!long messageBox;
    public uint length;

    this() {
        this.messageBox = DList!long();
        length = 0;
    }

    pragma(inline, true)
    public long dequeue() {
        auto message = messageBox.front;
        messageBox.removeFront;
        length--;
        return message;
    }

    pragma(inline, true)
    public void enqueue(long message) {
        messageBox.insertBack(message);
        length++;
    }
}
