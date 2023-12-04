import std.regex;
import std.conv;
import std.utf;
import std.array;
import std.stdio;
import std.string;

enum Opcode : ubyte {
    // Register machine opcodes
    jmprnze,
    jmpringt,
    subrri,
    subrsi,
    addrri,
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

enum OperandType : ubyte {
    stackValue,
    register,
    label,
    immediateValue,
    stackOffset,
    arity,
    returnMode,
    systemCall,
    string
}

alias StackValueType = long;
alias RegisterType = ubyte;
alias LabelType = uint;
alias AddressType = uint;
alias ImmediateValueType = long;
alias StackOffsetType = uint;
alias DataLengthType = ushort;
alias ArityType = ubyte;
alias ReturnModeType = ubyte;
alias SystemCallType = ushort;

struct OpcodeInfo {
    Opcode opcode;
    OperandType[] operandTypes;
}

enum SystemCall : SystemCallType {
    self,
    send,
    recv,
    println,
    display,
    exit
}

enum ReturnMode : ReturnModeType {
    value = 0,
    copy = 1
}

template Operand(T, string inputVar, string sizeVar) {
    enum Operand = `
        {
            ` ~ T.stringof ~ ` result = *cast(` ~ T.stringof ~ `*) ` ~ inputVar ~ `;
            ` ~ inputVar ~ ` += ` ~ T.stringof ~ `.sizeof;
            ` ~ sizeVar ~ ` += ` ~ T.stringof ~ `.sizeof;
            return result;
        }()
    `;
}

class VmError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
}

class Vm {
    public static const ubyte numberOfRegisters = 64;
    public static const OpcodeInfo[string] stringToOpcodeInfo;
    public static string[Opcode] opcodeToString;
    public static const SystemCall[string] stringToSystemCall;
    private static string[SystemCall] systemCallToString;
    private static const ReturnMode[string] stringToReturnMode;
    private static string[ReturnMode] returnModeToString;

    static this() {
        stringToOpcodeInfo = [
            // Register machine opcodes
            "jmprnze" : OpcodeInfo(Opcode.jmprnze,
                                   [OperandType.register,
                                    OperandType.label]),
            "jmpringt" : OpcodeInfo(Opcode.jmpringt,
                                    [OperandType.register,
                                     OperandType.immediateValue,
                                     OperandType.label]),
            "subrri" : OpcodeInfo(Opcode.subrri,
                                  [OperandType.register,
                                   OperandType.register,

                                   OperandType.immediateValue]),
            "subrsi" : OpcodeInfo(Opcode.subrsi,
                                  [OperandType.register,
                                   OperandType.stackOffset,
                                   OperandType.immediateValue]),
            "addrri" : OpcodeInfo(Opcode.addrri,
                                  [OperandType.register,
                                   OperandType.register,
                                   OperandType.immediateValue]),
            "loadri" : OpcodeInfo(Opcode.loadri,
                                  [OperandType.register,
                                   OperandType.immediateValue]),
            "pushr" : OpcodeInfo(Opcode.pushr,
                                 [OperandType.register]),
            "loadrs" : OpcodeInfo(Opcode.loadrs,
                                  [OperandType.register,
                                   OperandType.stackOffset]),
            "loadrr" : OpcodeInfo(Opcode.loadrr,
                                  [OperandType.register,
                                   OperandType.register]),
            "rcall" : OpcodeInfo(Opcode.rcall,
                                 [OperandType.label]),
            "rret" : OpcodeInfo(Opcode.rret,
                                []),
            "jmp" : OpcodeInfo(Opcode.jmp,
                               [OperandType.label]),
            // Stack machine opcodes
            "push" : OpcodeInfo(Opcode.push, [OperandType.stackValue]),
            "pushs" : OpcodeInfo(Opcode.pushs, [OperandType.string]),
            "pop" : OpcodeInfo(Opcode.pop, []),
            "dup" : OpcodeInfo(Opcode.dup, []),
            "swap" : OpcodeInfo(Opcode.swap, []),
            "load" : OpcodeInfo(Opcode.load, []),
            "store" : OpcodeInfo(Opcode.store, []),
            "add" : OpcodeInfo(Opcode.add, []),
            "sub" : OpcodeInfo(Opcode.sub, []),
            "mul" : OpcodeInfo(Opcode.mul, []),
            "div" : OpcodeInfo(Opcode.div, []),
            "jump" : OpcodeInfo(Opcode.jump, [OperandType.label]),
            "cjump" : OpcodeInfo(Opcode.cjump, [OperandType.label]),
            "call" : OpcodeInfo(Opcode.call,
                                [OperandType.label, OperandType.arity]),
            "ret" : OpcodeInfo(Opcode.ret, [OperandType.returnMode]),
            "sys" : OpcodeInfo(Opcode.sys, [OperandType.systemCall]),
            "and" : OpcodeInfo(Opcode.and, []),
            "or" : OpcodeInfo(Opcode.or, []),
            "not" : OpcodeInfo(Opcode.not, []),
            "eq" : OpcodeInfo(Opcode.eq, []),
            "neq" : OpcodeInfo(Opcode.neq, []),
            "lt" : OpcodeInfo(Opcode.lt, []),
            "gt" : OpcodeInfo(Opcode.gt, []),
            "nop" : OpcodeInfo(Opcode.nop, []),
            "halt" : OpcodeInfo(Opcode.halt, []),
            "mcall" : OpcodeInfo(Opcode.mcall, []),
            "spawn" : OpcodeInfo(Opcode.spawn,
                                 [OperandType.label, OperandType.arity]),
            "mspawn" : OpcodeInfo(Opcode.mspawn, []),
                              ];

        foreach (string, opcodeInfo; stringToOpcodeInfo) {
            opcodeToString[opcodeInfo.opcode] = string;
        }

        stringToSystemCall = [
            "self" : SystemCall.self,
            "send" : SystemCall.send,
            "recv": SystemCall.recv,
            "println" : SystemCall.println,
            "display" : SystemCall.display,
            "exit" : SystemCall.exit
        ];

        foreach (string, systemCall; stringToSystemCall) {
            systemCallToString[systemCall] = string;
        }

        stringToReturnMode = [
            "value" : ReturnMode.value,
            "copy" : ReturnMode.copy
        ];

        foreach (string, returnMode; stringToReturnMode) {
            returnModeToString[returnMode] = string;
        }
    }

    pragma(inline, true)
    public static T getValue(T)(ubyte* bytes) {
        return *cast(T*)bytes;
    }

    public static void setValue(T)(T value, ubyte* bytes) {
        *cast(T*)bytes = value;
    }
}
