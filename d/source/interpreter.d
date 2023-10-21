module interpreter;

import std.conv;
import std.datetime;
import std.stdio;
import std.algorithm.iteration;
import std.algorithm.mutation;
import std.array;
import std.range;

import job;
import scheduler;
import loader;
import prettyprint;
import instructions;

class InterpreterError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
}

enum InterpreterResult : ubyte {
    halt,
    timeout,
    recv,
    exit
}

class Interpreter {
    Loader loader;

    this(Loader loader) {
        this.loader = loader;
    }

    InterpreterResult run(Scheduler scheduler, Job job,
                          Duration timeSlice, uint checkAfter) {
        auto startTime = Clock.currTime();
        uint instructionsExecuted = 0;
        InterpreterResult interpreterResult;

        while (true) {
            auto byteCode = loader.byteCode;

            debug(interpreter) {
                writefln("%d: r0 = %d, r1 = %d, r2 = %d",
                         job.jid,
                         job.registers[0],
                         job.registers[1],
                         job.registers[2]);
                writefln("%d: %s", job.jid, to!string(job.callStack.stack));
                writef("%d:%d: ", job.jid, job.pc);
                PrettyPrint.printInstruction(&byteCode[job.pc]);
            }

            auto currentPc = job.pc;

            if (++job.pc > byteCode.length) {
                throw new InterpreterError(
                              "Unexpected end of bytecode or invalid jump");
            }

            switch (byteCode[currentPc]) {
            // Register machine instructions
            case Opcodes.jmprnze:
                auto register = Instructions.get!ubyte(&byteCode[job.pc]);
                if (job.registers[register] != 0) {
                    job.pc =
                        Instructions.get!uint(&byteCode[job.pc + ubyte.sizeof]);
                } else {
                    job.pc += ubyte.sizeof + uint.sizeof;
                }
                break;
            case Opcodes.jmpringt:
                /*
                '(register, immediateValue, address) =
                    registerValueAddress(job.pc, byte_code);
                if (!(job.registers[register] > immediateValue)) {
                    job.pc = get!ubyte(&byteCode[job.pc + rilSize]);
                } else {
                    job.pc += rilSize;
                }
                */





                auto register = Instructions.get!ubyte(&byteCode[job.pc]);
                auto value =
                    Instructions.get!long(&byteCode[job.pc + ubyte.sizeof]);
                if (!(job.registers[register] > value)) {
                    job.pc = Instructions.get!ubyte(
                                 &byteCode[job.pc + ubyte.sizeof +
                                           long.sizeof]);
                } else {
                    job.pc += ubyte.sizeof + ulong.sizeof + uint.sizeof;
                }
                break;
            case Opcodes.subrri:
                auto firstRegister = Instructions.get!ubyte(&byteCode[job.pc]);
                auto secondRegister =
                    Instructions.get!ubyte(&byteCode[job.pc + ubyte.sizeof]);
                auto value =
                    Instructions.get!long(
                        &byteCode[job.pc + ubyte.sizeof + ubyte.sizeof]);
                job.registers[firstRegister] =
                    job.registers[secondRegister] - value;
                job.pc += ubyte.sizeof + ubyte.sizeof + long.sizeof;
                break;


            case Opcodes.subrsi:
                auto register = Instructions.get!ubyte(&byteCode[job.pc]);
                auto stackOffset =
                    Instructions.get!ubyte(&byteCode[job.pc + ubyte.sizeof]);
                auto value =
                    Instructions.get!long(
                        &byteCode[job.pc + ubyte.sizeof + uint.sizeof]);
                job.registers[register] =
                    job.callStack.stack[job.callStack.fp + stackOffset] -
                    value;
                job.pc += ubyte.sizeof + uint.sizeof + long.sizeof;
                break;




            case Opcodes.addrri:
                auto firstRegister = Instructions.get!ubyte(&byteCode[job.pc]);
                auto secondRegister =
                    Instructions.get!ubyte(&byteCode[job.pc + ubyte.sizeof]);
                auto value =
                    Instructions.get!long(
                        &byteCode[job.pc + ubyte.sizeof + ubyte.sizeof]);
                job.registers[firstRegister] =
                    job.registers[secondRegister] + value;
                job.pc += ubyte.sizeof + ubyte.sizeof + long.sizeof;
                break;
            case Opcodes.loadri:
                auto register = Instructions.get!ubyte(&byteCode[job.pc]);
                auto value = Instructions.get!long(
                                 &byteCode[job.pc + ubyte.sizeof]);
                job.registers[register] = value;
                job.pc += ubyte.sizeof + long.sizeof;
                break;
            case Opcodes.pushr:
                auto register = Instructions.get!ubyte(&byteCode[job.pc]);
                job.callStack.push(job.registers[register]);
                job.pc += ubyte.sizeof;
                break;
            case Opcodes.loadrs:
                auto register = Instructions.get!ubyte(&byteCode[job.pc]);
                auto stackOffset = Instructions.get!uint(
                                 &byteCode[job.pc + ubyte.sizeof]);
                job.registers[register] =
                    job.callStack.stack[job.callStack.fp + stackOffset];
                job.pc += ubyte.sizeof + uint.sizeof;
                break;
            case Opcodes.loadrr:
                auto firstRegister = Instructions.get!ubyte(&byteCode[job.pc]);
                auto secondRegister =
                    Instructions.get!ubyte(&byteCode[job.pc + ubyte.sizeof]);
                job.registers[firstRegister] = job.registers[secondRegister];
                job.pc += ubyte.sizeof + ubyte.sizeof;
                break;





            case Opcodes.rcall:
                // Extract address to function
                auto address = Instructions.get!uint(&byteCode[job.pc]);
                // Push return address onto call stack
                job.callStack.push(job.pc + uint.sizeof);
                // Push previous FP onto call stack
                job.callStack.push(job.callStack.fp);
                // Set FP to point at return address
                job.callStack.fp = job.callStack.length - 2;
                // Jump to function address
                job.pc = address;
                break;
            case Opcodes.rret:
                auto returnAddress = job.callStack.stack[job.callStack.fp];
                // Remember previous FP
                auto previousFp = job.callStack.stack[job.callStack.fp + 1];
                // Remove call stack frame
                job.callStack.stack =
                    job.callStack.stack[0 .. job.callStack.fp];
                // Has call stack been exhausted?
                if (job.callStack.length == 1 || returnAddress == -1) {
                    return InterpreterResult.halt;
                }
                // Restore FP to previous FP
                job.callStack.fp = previousFp;
                // Jump to return address
                job.pc = cast(uint)returnAddress;
                break;
            case Opcodes.jmp:
                auto address = Instructions.get!uint(&byteCode[job.pc]);
                job.pc = address;
                break;



            // Stack machine instructions
            case Opcodes.push:
                auto value = Instructions.get!long(&byteCode[job.pc]);
                job.callStack.push(value);
                job.pc += long.sizeof;
                break;
            case Opcodes.pushs:
                auto result = job.dataStack.push(byteCode[job.pc .. $]);
                auto dataAddress = result[0];
                auto length = result[1];
                job.callStack.push(dataAddress);
                job.pc += ushort.sizeof + length;
                break;
            case Opcodes.pop:
                job.callStack.pop();
                break;
            case Opcodes.dup:
                job.callStack.dup();
                break;
            case Opcodes.swap:
                job.callStack.swap();
                break;
            case Opcodes.load:
                job.callStack.load();
                break;
            case Opcodes.store:
                job.callStack.store();
                break;
            case Opcodes.add:
                job.callStack.op((operand1, operand2) => operand1 + operand2);
                break;
            case Opcodes.sub:
                job.callStack.op((operand1, operand2) => operand1 - operand2);
                break;
            case Opcodes.mul:
                job.callStack.op((operand1, operand2) => operand1 * operand2);
                break;
            case Opcodes.div:
                job.callStack.op((operand1, operand2) => operand1 / operand2);
                break;
            case Opcodes.jump:
                auto address = Instructions.get!uint(&byteCode[job.pc]);
                job.pc = address;
                break;
            case Opcodes.cjump:
                auto address = Instructions.get!uint(&byteCode[job.pc]);
                auto conditional = job.callStack.pop();
                if (conditional != 0) {
                    job.pc = address;
                } else {
                    job.pc += uint.sizeof;
                }
                break;
            case Opcodes.call:
                // Extract address to function and its arity
                auto address = Instructions.get!uint(&byteCode[job.pc]);
                auto arity =
                    Instructions.get!ubyte(&byteCode[job.pc + uint.sizeof]);
                call(job, address, arity, uint.sizeof + ubyte.sizeof);
                break;
            case Opcodes.mcall:
                // Extract function label, module name and function arity
                auto label = job.callStack.pop();
                auto moduleName = job.callStack.popString();
                auto arity = job.callStack.pop();
                // Ensure that module is loaded
                if (!loader.isModuleLoaded(moduleName)) {
                    loader.loadModule(moduleName);
                    debug(scheduler) {
                        loader.prettyPrint(moduleName);
                    }
                }
                auto address =
                    loader.lookupAddress(moduleName, cast(uint)label);
                call(job, cast(uint)address, cast(ubyte)arity, 0);
                break;
            case Opcodes.ret:
                // Remember essential stack information
                auto returnValue = job.callStack.pop();
                auto arity = job.callStack.stack[job.callStack.fp];
                auto returnAddress = job.callStack.stack[job.callStack.fp + 1];
                auto previousFp = job.callStack.stack[job.callStack.fp + 2];
                // Remove stack frame
                job.callStack.stack =
                    job.callStack.stack[0 .. job.callStack.fp - arity];
                // Push return value onto caller's stack
                auto returnMode = Instructions.get!ubyte(&byteCode[job.pc]);
                if (returnMode == ReturnModes.copy) {
                    ubyte[] returnData = job.dataStack.peek(returnValue);
                    // Copy data onto caller's data stack
                    auto returnDataCopy = job.dataStack.push(returnData);
                    auto dataAddress = returnDataCopy[0];
                    // Push data address onto caller's stack
                    job.callStack.push(dataAddress);
                } else {
                    // Push return value onto caller's stack
                    job.callStack.push(returnValue);
                }
                // Has stack been exhausted?
                if (job.callStack.length == 1 || returnAddress == -1) {
                    // Just keep result
                    job.callStack.stack = job.callStack.stack[$ - 1 .. $];
                    return InterpreterResult.halt;
                }
                // Remove data stack frame
                auto previousDataFp =
                    Instructions.get!long(
                        &job.dataStack.stack[job.dataStack.fp]);
                job.dataStack.stack =
                    job.dataStack.stack[0 .. job.dataStack.fp];
                // Restore FP and data FP
                job.callStack.fp = previousFp;
                job.dataStack.fp = previousDataFp;
                // Jump to return address
                job.pc = cast(uint)returnAddress;
                break;
            case Opcodes.sys:
                auto systemCall = Instructions.get!ushort(&byteCode[job.pc]);
                final switch (systemCall) {
                case SystemCalls.self:
                    job.callStack.push(job.jid);
                    break;
                case SystemCalls.send:
                    auto value = job.callStack.pop();
                    auto jid = job.callStack.pop();
                    scheduler.sendMessage(cast(uint)jid, value);
                    job.callStack.push(value);
                    break;
                case SystemCalls.recv:
                    if (job.messageBox.length == 0) {
                        --job.pc;
                        return InterpreterResult.recv;
                    }
                    job.callStack.push(job.messageBox.dequeue());
                    break;
                case SystemCalls.println:
                    auto s = job.callStack.popString();
                    writeln(s);
                    job.callStack.push(1);
                    break;
                case SystemCalls.display:
                    auto topValue = job.callStack.pop();
                    writefln("%d", topValue);
                    job.callStack.push(1);
                    break;
                case SystemCalls.exit:
                    return InterpreterResult.exit;
                }
                job.pc += ushort.sizeof;
                break;
            case Opcodes.and:
                job.callStack.op((operand1, operand2) =>
                                 (operand1 != 0) && (operand2 == 0) ? 1 : 0);
                break;
            case Opcodes.or:
                job.callStack.op((operand1, operand2) =>
                                 (operand1 != 0) || (operand2 != 0) ? 1 : 0);
                break;
            case Opcodes.not:
                auto operand = job.callStack.pop();
                job.callStack.push(!(operand != 0) ? 1 : 0);
                break;
            case Opcodes.eq:
                job.callStack.op((operand1, operand2) =>
                                 (operand1 == operand2) ? 1 : 0);
                break;
            case Opcodes.neq:
                job.callStack.op((operand1, operand2) =>
                                 (operand1 != operand2) ? 1 : 0);
                break;
            case Opcodes.lt:
                job.callStack.op((operand1, operand2) =>
                                 operand1 < operand2 ? 1 : 0);
                break;
            case Opcodes.gt:
                job.callStack.op((operand1, operand2) =>
                                 operand1 > operand2 ? 1 : 0);
                break;
            case Opcodes.nop:
                break;
            case Opcodes.halt:
                return InterpreterResult.halt;
            case Opcodes.spawn:
                auto address = Instructions.get!uint(&byteCode[job.pc]);
                auto arity =
                    Instructions.get!ubyte(&byteCode[job.pc + uint.sizeof]);
                auto parameters =
                    iota(arity).map!(_ => job.callStack.pop()).array.reverse;
                auto jid = spawn(scheduler, address, parameters);
                job.callStack.push(jid);
                job.pc += uint.sizeof + ubyte.sizeof;
                break;
            case Opcodes.mspawn:
                auto label = job.callStack.pop();
                auto moduleName = job.callStack.popString();
                auto arity = job.callStack.pop();
                auto parameters =
                    iota(arity).map!(_ => job.callStack.pop()).array.reverse;
                auto jid = mspawn(loader, scheduler, moduleName,
                                  cast(uint)label, parameters);
                job.callStack.push(jid);
                break;
            default:
                throw new InterpreterError(
                              "Invalid opcode " ~
                              to!string(byteCode[currentPc]));
            }

            if (instructionsExecuted ++ >= checkAfter) {
                if (Clock.currTime() - startTime >= timeSlice) {
                    interpreterResult = InterpreterResult.timeout;
                    break;
                }
                instructionsExecuted = 0;
            }
        }

        return interpreterResult;
    }

    static uint spawn(Scheduler scheduler, uint address, long[] parameters) {
        long arity = parameters.length;
        long returnAddress = -1;
        long fp = -1;
        long[] initialCallStack = parameters ~ arity ~ returnAddress ~ fp;
        auto jid = Scheduler.nextJid();
        auto job = new Job(jid, address, initialCallStack);
        // Set FP to point to arity
        job.callStack.fp = job.callStack.length - 3;
        scheduler.spawn(job);
        return jid;
    }

    static uint mspawn(Loader loader, Scheduler scheduler, string moduleName,
                       uint label, long[] parameters) {
        // Ensure that module is loaded
        if (!loader.isModuleLoaded(moduleName)) {
            loader.loadModule(moduleName);
            debug(scheduler) {
                loader.prettyPrint(moduleName);
            }
        }
        auto address = loader.lookupAddress(moduleName, label);
        return spawn(scheduler, address, parameters);
    }

    void call(Job job, uint address, ubyte arity, ubyte sizeOfOperands) {
        // Push arity onto stack
        job.callStack.push(arity);
        // Push return address onto stack
        job.callStack.push(job.pc + sizeOfOperands);
        // Push previous FP onto stack
        job.callStack.push(job.callStack.fp);
        // Set FP to point to arity
        job.callStack.fp = job.callStack.length - 3;
        // Save previous data FP on data stack
        Instructions.insert(job.dataStack.fp, job.dataStack.stack);
        // Set data FP to previous data FP
        job.dataStack.fp = job.dataStack.length - long.sizeof;
        // Jump to function address
        job.pc = address;
    }
}
