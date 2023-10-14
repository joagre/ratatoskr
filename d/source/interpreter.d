module interpreter;

import std.conv : to;
import std.datetime : Duration, Clock;
import std.stdio : write, writeln, writefln, writef;
import std.algorithm.iteration : map;
import std.algorithm.mutation : reverse;
import std.array;
import std.range : iota;

import job;
import scheduler;
import loader;
import prettyprint;

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
                writefln("%d: %s", job.jid, to!string(job.callStack.stack));
                writef("%d: ", job.jid);
                PrettyPrint.printInstruction(&byteCode[job.pc]);
            }

            //Thread.sleep(dur!"msecs"(50));
            //readln();

            auto currentPc = job.pc;

            if (++job.pc > byteCode.length) {
                throw new InterpreterError(
                              "Unexpected end of bytecode or invalid jump");
            }

            switch (byteCode[currentPc] >> OPCODE_BITS) {
            case Opcodes.push:
                auto value = Loader.get!long(&byteCode[job.pc]);
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
                auto register =
                    cast(ubyte)(byteCode[currentPc] & OPCODE_OPERAND_MASK);
                job.callStack.load(register);
                break;
            case Opcodes.store:
                auto register =
                    cast(ubyte)(byteCode[currentPc] & OPCODE_OPERAND_MASK);
                job.callStack.store(register);
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
                auto byteIndex = Loader.get!uint(&byteCode[job.pc]);
                job.pc = byteIndex;
                break;
            case Opcodes.cjump:
                auto byteIndex = Loader.get!uint(&byteCode[job.pc]);
                auto conditional = job.callStack.pop();
                if (conditional != 0) {
                    job.pc = byteIndex;
                } else {
                    job.pc += uint.sizeof;
                }
                break;
            case Opcodes.call:
                // Extract call operands
                auto byteIndex = Loader.get!uint(&byteCode[job.pc]);
                auto arity = Loader.get!ubyte(&byteCode[job.pc + uint.sizeof]);
                call(job, byteIndex, arity);
                break;
            case Opcodes.mcall:
                // Extract call parameters from stack (keep parameters on stack)
                auto label = job.callStack.pop();
                auto moduleName = job.popString();
                auto arity = job.callStack.pop();
                // Ensure that the module is loaded
                if (!loader.isModuleLoaded(moduleName)) {
                    loader.loadPOSMCode(moduleName);
                    debug(scheduler) {
                        loader.prettyPrint(moduleName);
                    }
                }
                auto byteIndex =
                    loader.lookupByteIndex(moduleName, cast(uint)label);
                call(job, cast(uint)byteIndex, cast(ubyte)arity);
                break;
            case Opcodes.ret:
                // Is the return done by value or by copy?
                auto returnMode =
                    cast(ubyte)(byteCode[currentPc] & OPCODE_OPERAND_MASK);
                // Swap return value and previous fp
                job.callStack.swap();
                // Restore fp to previous fp
                auto currentFp = job.callStack.fp;
                job.callStack.fp = job.callStack.pop();
                // Swap return value and return address
                job.callStack.swap();
                // Pop return address
                auto returnAddress = job.callStack.pop();
                // Has stack been exhausted?
                if (job.callStack.fp == -1) {
                    // Just keep the result and throw away any parameters
                    job.callStack.stack = job.callStack.stack[$ - 1 .. $];
                    return InterpreterResult.halt;
                }
                // Pop return value
                auto returnValue = job.callStack.pop();
                ubyte[] returnData = null;
                if (returnMode == ReturnModes.copy) {
                    // Extract return data
                    returnData = job.dataStack.peek(returnValue);
                }
                // Remove call stack frame
                job.callStack.stack = job.callStack.stack[0 .. currentFp];
                // Jump to return address
                job.pc = cast(uint)returnAddress;
                // Remove data stack frame
                auto previousDataFp =
                    Loader.get!long(&job.dataStack.stack[job.dataStack.fp]);
                job.dataStack.stack =
                    job.dataStack.stack[0 .. job.dataStack.fp];
                // Restore data fp to previous data fp
                job.dataStack.fp = previousDataFp;
                // Reinsert return value on call stack (and data stack)
                if (returnMode == ReturnModes.copy) {
                    // Copy the return data onto the caller's data stack
                    auto result = job.dataStack.push(returnData);
                    auto dataAddress = result[0];
                    // Push the return data address onto caller's call stack
                    job.callStack.push(dataAddress);
                } else {
                    // Push the return value onto caller's call stack
                    job.callStack.push(returnValue);
                }
                break;
            case Opcodes.sys:
                auto systemCall = Loader.get!uint(&byteCode[job.pc]);
                switch (systemCall) {
                case SystemCalls.recv:
                    return InterpreterResult.recv;
                case SystemCalls.println:
                    auto s = job.popString();
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
                default:
                    throw new InterpreterError("SYS is not implemented");
                }
                job.pc += uint.sizeof;
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
                auto byteIndex = Loader.get!uint(&byteCode[job.pc]);
                auto arity = Loader.get!ubyte(&byteCode[job.pc + uint.sizeof]);
                auto parameters =
                    iota(arity).map!(_ => job.callStack.pop()).array;
                auto jid = scheduler.spawn(byteIndex, parameters.reverse);
                job.callStack.push(jid);
                break;
            case Opcodes.mspawn:
                auto label = job.callStack.pop();
                auto moduleName = job.popString();
                auto arity = job.callStack.pop();
                auto parameters =
                    iota(arity).map!(_ => job.callStack.pop()).array;
                auto jid = scheduler.mspawn(moduleName,
                                            cast(uint)label, parameters);
                job.callStack.push(jid);
                break;
            default:
                throw new InterpreterError(
                              "Invalid opcode" ~
                              to!string(byteCode[currentPc] >> OPCODE_BITS));
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

    void call(Job job, uint byteIndex, ubyte arity) {
        // Add return address to stack
        job.callStack.push(job.pc + uint.sizeof + ubyte.sizeof);
        // Save previous fp on the stack
        job.callStack.push(job.callStack.fp);
        // Set fp to first CALL parameter (NOTE: We just pushed two values)
        job.callStack.fp = job.callStack.length - 2 - arity;
        // Jump to CALL byte index
        job.pc = byteIndex;
        // Save previous data fp on the data stack
        Loader.insert(job.dataStack.fp, job.dataStack.stack);
        // Set data fp to the previous data fp
        job.dataStack.fp = job.dataStack.length - long.sizeof;
    }
}
