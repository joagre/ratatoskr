module job;

import std.stdio;
import std.conv;
import std.typecons;
import std.container;

import vm;

enum JobMode : ubyte {
    init,
    running,
    ready,
    waiting
}

class Job {
    public uint jid;
    public JobMode mode;
    public AddressType pc;
    public DataStack dataStack;
    public CallStack callStack;
    public MessageBox messageBox;
    public long[Vm.numberOfRegisters] registers;

    this(uint jid, AddressType pc, long[] initialCallStack) {
        this.jid = jid;
        this.mode = JobMode.init;
        this.pc = pc;
        this.dataStack = new DataStack;
        this.callStack = new CallStack(initialCallStack, this.dataStack);
        this.messageBox = new MessageBox;
    }
}

Please convert this D class to a typedefed C struct. All variables must be in snake case and every type must end in _t. D's dynamic arrays should be converted to the functionality in dynarray.h I provided earlier. Ignore pragma:

class CallStack  {
    public long[] stack;
    public long fp;
    public DataStack dataStack;

    this(long[] initialCallStack, DataStack dataStack) {
        this.stack = initialCallStack;
        this.fp = 0;
        this.dataStack = dataStack;
    }

    public long length() {
        return stack.length;
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
    public void load() {
        auto offset = pop();
        push(stack[fp + offset]);
    }

    pragma(inline, true)
    public void store() {
        auto offset = pop();
        auto newValue = pop();
        stack[fp + offset] = newValue;
    }

    pragma(inline, true)
    public void binaryOperation(long delegate(long, long) fun) {
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

    public long length() {
        return stack.length;
    }

    pragma(inline, true)
    public Tuple!(long, ushort) push(ubyte[] bytes) {
        auto length = Vm.getValue!DataLengthType(&bytes[0]);
        long dataAddress = stack.length;
        stack ~= bytes[0 .. DataLengthType.sizeof + length];
        return Tuple!(long, DataLengthType)(dataAddress, length);
    }

    pragma(inline, true)
    public ubyte[] peek(long dataAddress) {
        ubyte[] bytes = stack[dataAddress .. $];
        auto length = Vm.getValue!ushort(&bytes[0]);
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

    public long dequeue() {
        auto message = messageBox.front;
        messageBox.removeFront;
        length--;
        return message;
    }

    public void enqueue(long message) {
        messageBox.insertBack(message);
        length++;
    }
}
