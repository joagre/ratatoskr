module interpreter;

import std.conv : to;
import std.datetime : Duration, Clock;
import std.stdio : write, writeln;
import std.algorithm.iteration : map;
import std.algorithm.mutation : reverse;
import std.array;

import std.range : iota;
import program;
import job;
import scheduler;

class InterpreterError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
}

enum InterpreterResult {
    halt,
    timeout,
    recv
}

struct Interpreter {
    InterpreterResult run(ref Scheduler scheduler, ref Job job,
                          Duration timeSlice, uint timeoutGranularity) {
        auto startTime = Clock.currTime();
        uint instructionsExecuted = 0;
        InterpreterResult interpreterResult;

        while (true) {
            auto byteCode = job.program.byteCode;

            debug(interpreter) {
                write(to!string(job.jid) ~ ": ");
                writeln(job.callStack.stack);
                write(to!string(job.jid) ~ ": ");
                job.program.prettyPrint(&byteCode[job.pc], true);
            }

            //Thread.sleep(dur!"msecs"(50));
            //readln();

            auto currentPc = job.pc;

            if (++job.pc > byteCode.length) {
                throw new InterpreterError(
                              "Unexpected end of bytecode or invalid jump");
            }

            switch (byteCode[currentPc] >> 3) {
            case Opcodes.push:
                auto value = get!long(&byteCode[job.pc]);
                job.callStack.push(value);
                job.pc += 8;
                break;
            case Opcodes.pushs:
                auto result = job.dataStack.push(byteCode[job.pc .. $]);
                auto dataAddress = result[0];
                auto length = result[1];
                job.callStack.push(dataAddress);
                job.pc += 4 + length;
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
                auto register = cast(ubyte)(byteCode[currentPc] & 0b00000111);
                job.callStack.load(register);
                break;
            case Opcodes.store:
                auto register = cast(ubyte)(byteCode[currentPc] & 0b00000111);
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
                auto byteIndex = get!long(&byteCode[job.pc]);
                job.pc = byteIndex;
                break;
            case Opcodes.cjump:
                auto byteIndex = get!long(&byteCode[job.pc]);
                auto conditional = job.callStack.pop();
                if (conditional != 0) {
                    job.pc = byteIndex;
                } else {
                    job.pc += 8;
                }
                break;
            case Opcodes.call:
                // Extract call operands
                int byteIndex = get!int(&byteCode[job.pc]);
                int arity = get!int(&byteCode[job.pc + 4]);
                // Add return address to stack
                job.callStack.push(job.pc + 8);
                // Save previous fp on the stack
                job.callStack.push(job.callStack.fp);
                // Set fp to first parameter CALL parameter
                job.callStack.fp = job.callStack.length - 2 - arity;
                // Jump to CALL byte index
                job.pc = byteIndex;
                // Save previous data fp on the data stack
                insert(job.dataStack.fp, job.dataStack.stack);
                // Set data fp to the previous data fp
                job.dataStack.fp = job.dataStack.length - 8;
                break;
            case Opcodes.ret:
                // Is the return done by value or by copy?
                auto returnMode = cast(ubyte)(byteCode[currentPc] & 0b00000111);
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
                if (job.callStack.fp == -1 || returnAddress == 0) {
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
                job.pc = returnAddress;
                // Remove data stack frame
                auto previousDataFp =
                    get!long(&job.dataStack.stack[job.dataStack.fp]);
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
                auto systemCall = get!long(&byteCode[job.pc]);
                switch (systemCall) {
                case SystemCalls.spawn:
                    auto n = job.callStack.pop();
                    auto parameters =
                        iota(n).map!(_ => job.callStack.pop()).array;
                    auto filename = job.popString();
                    auto jid = scheduler.spawn(filename, parameters.reverse);
                    job.callStack.push(jid);
                    break;
                case SystemCalls.recv:
                    return InterpreterResult.recv;
                case SystemCalls.println:
                    auto s = job.popString();
                    writeln(s);
                    job.callStack.push(1);
                    break;
                case SystemCalls.display:
                    auto topValue = job.callStack.pop();
                    writeln("DISPLAY: " ~ to!string(topValue));
                    job.callStack.push(1);
                    break;
                default:
                    throw new InterpreterError("SYS is not implemented");
                }
                job.pc += 8;
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
            default:
                throw new InterpreterError("Invalid opcode" ~
                                           to!string(byteCode[currentPc] >> 3));
            }

            if (instructionsExecuted ++ >= timeoutGranularity) {
                if (Clock.currTime() - startTime >= timeSlice) {
                    interpreterResult = InterpreterResult.timeout;
                    break;
                }
                instructionsExecuted = 0;
            }
        }

        return interpreterResult;
    }
}
