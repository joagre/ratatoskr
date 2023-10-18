enum Opcodes : ubyte {
    // Register machine opcodes
    jmprnze,
    jmpringt,
    subrri,
    loadri,
    pushr,
    loadrs,
    loadrr,
    rcall,
    rret,
    jmp,
    // Stack machine opcodes
    push,
    pushs,
    pop,
    dup,
    swap,
    load,
    store,
    add,
    sub,
    mul,
    div,
    jump,
    cjump,
    call,
    ret,
    sys,
    and,
    or,
    not,
    eq,
    neq,
    lt,
    gt,
    nop,
    halt,
    mcall,
    spawn,
    mspawn
}

enum SystemCalls : ushort {
    self,
    send,
    recv,
    println,
    display,
    exit
}

enum ReturnModes : ubyte {
    value = 0,
    copy = 1
}

class Instructions {
    static string[Opcodes] opcodeToString;
    static Opcodes[string] stringToOpcode;
    static string[SystemCalls] systemCallToString;
    static SystemCalls[string] stringToSystemCall;
    static string[ReturnModes] returnModeToString;
    static ReturnModes[string] stringToReturnMode;

    static this() {
        string[Opcodes] opcodeToString = [
            // String machine opcodes
            Opcodes.jmprnze : "jmprnze",
            Opcodes.jmpringt : "jmpringt",
            Opcodes.subrri : "subrri",
            Opcodes.loadri : "loadri",
            Opcodes.pushr : "pushr",
            Opcodes.loadrs : "loadrs",
            Opcodes.loadrr : "loadrr",
            Opcodes.jmp : "jmp",
            // Stack machine opcodes
            Opcodes.push : "push",
            Opcodes.pushs : "pushs",
            Opcodes.pop : "pop",
            Opcodes.dup : "dup",
            Opcodes.swap : "swap",
            Opcodes.load : "load",
            Opcodes.store : "store",
            Opcodes.add : "add",
            Opcodes.sub : "sub",
            Opcodes.mul : "mul",
            Opcodes.div : "div",
            Opcodes.jump : "jump",
            Opcodes.cjump : "cjump",
            Opcodes.call : "call",
            Opcodes.ret : "ret",
            Opcodes.sys : "sys",
            Opcodes.and : "and",
            Opcodes.or : "or",
            Opcodes.not : "not",
            Opcodes.eq : "eq",
            Opcodes.neq : "neq",
            Opcodes.lt : "lt",
            Opcodes.gt : "gt",
            Opcodes.nop : "nop",
            Opcodes.halt : "halt",
            Opcodes.mcall : "mcall",
            Opcodes.spawn : "spawn",
            Opcodes.mspawn : "mspawn"
        ];

        foreach (opcode, string; opcodeToString) {
            stringToOpcode[string] = opcode;
        }

        string[SystemCalls] systemCallToString = [
            SystemCalls.self : "self",
            SystemCalls.send : "send",
            SystemCalls.recv : "recv",
            SystemCalls.println : "println",
            SystemCalls.display : "display",
            SystemCalls.exit : "exit"
        ];

        foreach (systemCall, string; systemCallToString) {
            stringToSystemCall[string] = systemCall;
        }

        string[ReturnModes] returnModeToString = [
            ReturnModes.value : "value",
            ReturnModes.copy : "copy"
        ];

        foreach (returnMode, string; returnModeToString) {
            stringToReturnMode[string] = returnMode;
        }
    }

    static void insert(T)(T value, ref ubyte[] bytes) {
        bytes ~= (cast(ubyte*)&value)[0 .. T.sizeof];
    }

    pragma(inline, true)
    static T get(T)(ubyte* bytes) {
        return *cast(T*)bytes;
    }

    static void set(T)(T value, ubyte* bytes) {
        *cast(T*)bytes = value;
    }
}
