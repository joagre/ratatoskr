module interpreter;

import std.conv : to;
import std.datetime : Duration, Clock;
import std.stdio : write, writeln;
import std.algorithm.iteration : map;
import std.algorithm.mutation : reverse;
import std.array;

import std.range : iota;
import program;
import runcontext;
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
    InterpreterResult run(ref Scheduler scheduler, ref RunContext runContext,
                          Duration timeSlice, uint timeoutGranularity) {
        auto startTime = Clock.currTime();
        uint instructionsExecuted = 0;
        InterpreterResult interpreterResult;

        while (true) {
            auto byteCode = runContext.program.byteCode;

            debug(interpreter) {
                write(to!string(runContext.rcid) ~ ": ");
                writeln(runContext.callStack.stack);
                write(to!string(runContext.rcid) ~ ": ");
                runContext.program.prettyPrint(&byteCode[runContext.pc], true);
            }

            //Thread.sleep(dur!"msecs"(50));
            //readln();

            auto currentPc = runContext.pc;

            if (++runContext.pc > byteCode.length) {
                throw new InterpreterError(
                              "Unexpected end of bytecode or invalid jump");
            }

            switch (byteCode[currentPc] >> 3) {
            case Opcodes.push:
                auto value = get!long(&byteCode[runContext.pc]);
                runContext.callStack.push(value);
                runContext.pc += 8;
                break;
            case Opcodes.pushs:
                auto result =
                    runContext.dataStack.push(byteCode[runContext.pc .. $]);
                auto dataAddress = result[0];
                auto length = result[1];
                runContext.callStack.push(dataAddress);
                runContext.pc += 4 + length;
                break;
            case Opcodes.pop:
                runContext.callStack.pop();
                break;
            case Opcodes.dup:
                runContext.callStack.dup();
                break;
            case Opcodes.swap:
                runContext.callStack.swap();
                break;
            case Opcodes.load:
                auto register = cast(ubyte)(byteCode[currentPc] & 0b00000111);
                runContext.callStack.load(register);
                break;
            case Opcodes.store:
                auto register = cast(ubyte)(byteCode[currentPc] & 0b00000111);
                runContext.callStack.store(register);
                break;
            case Opcodes.add:
                runContext.callStack.op((operand1, operand2) =>
                                        operand1 + operand2);
                break;
            case Opcodes.sub:
                runContext.callStack.op((operand1, operand2) =>
                                        operand1 - operand2);
                break;
            case Opcodes.mul:
                runContext.callStack.op((operand1, operand2) =>
                                        operand1 * operand2);
                break;
            case Opcodes.div:
                runContext.callStack.op((operand1, operand2) =>
                                        operand1 / operand2);
                break;
            case Opcodes.jump:
                auto byteIndex = get!long(&byteCode[runContext.pc]);
                runContext.pc = byteIndex;
                break;
            case Opcodes.cjump:
                auto byteIndex = get!long(&byteCode[runContext.pc]);
                auto conditional = runContext.callStack.pop();
                if (conditional != 0) {
                    runContext.pc = byteIndex;
                } else {
                    runContext.pc += 8;
                }
                break;
            case Opcodes.call:
                // Extract call operands
                int byteIndex = get!int(&byteCode[runContext.pc]);
                int arity = get!int(&byteCode[runContext.pc + 4]);
                // Add return address to stack
                runContext.callStack.push(runContext.pc + 8);
                // Save previous fp on the stack
                runContext.callStack.push(runContext.callStack.fp);
                // Set fp to first parameter CALL parameter
                runContext.callStack.fp = runContext.callStack.length - 2 - arity;
                // Jump to CALL byte index
                runContext.pc = byteIndex;
                // Save previous data fp on the data stack
                insert(runContext.dataStack.fp, runContext.dataStack.stack);
                // Set data fp to the previous data fp
                runContext.dataStack.fp = runContext.dataStack.length - 8;
                break;
            case Opcodes.ret:
                // Is the return done by value or by copy?
                auto returnMode = cast(ubyte)(byteCode[currentPc] & 0b00000111);
                // Swap return value and previous fp
                runContext.callStack.swap();
                // Restore fp to previous fp
                auto currentFp = runContext.callStack.fp;
                runContext.callStack.fp = runContext.callStack.pop();
                // Swap return value and return address
                runContext.callStack.swap();
                // Pop return address
                auto returnAddress = runContext.callStack.pop();
                // Has stack been exhausted?
                if (runContext.callStack.fp == -1 || returnAddress == 0) {
                    return InterpreterResult.halt;
                }
                // Pop return value
                auto returnValue = runContext.callStack.pop();
                ubyte[] returnData = null;
                if (returnMode == ReturnModes.copy) {
                    // Extract return data
                    returnData = runContext.dataStack.peek(returnValue);
                }
                // Remove call stack frame
                runContext.callStack.stack =
                    runContext.callStack.stack[0 .. currentFp];
                // Jump to return address
                runContext.pc = returnAddress;
                // Remove data stack frame
                auto previousDataFp =
                    get!long(&runContext.dataStack.stack[
                                 runContext.dataStack.fp]);
                runContext.dataStack.stack =
                    runContext.dataStack.stack[0 .. runContext.dataStack.fp];
                // Restore data fp to previous data fp
                runContext.dataStack.fp = previousDataFp;
                // Reinsert return value on call stack (and data stack)
                if (returnMode == ReturnModes.copy) {
                    // Copy the return data onto the caller's data stack
                    auto result = runContext.dataStack.push(returnData);
                    auto dataAddress = result[0];
                    // Push the return data address onto caller's call stack
                    runContext.callStack.push(dataAddress);
                } else {
                    // Push the return value onto caller's call stack
                    runContext.callStack.push(returnValue);
                }
                break;
            case Opcodes.sys:
                auto systemCall = get!long(&byteCode[runContext.pc]);
                switch (systemCall) {
                case SystemCalls.spawn:
                    auto n = runContext.callStack.pop();
                    auto parameters = iota(n).map!(_ => runContext.callStack.pop()).array;
                    auto filename = runContext.popString();
                    auto rcid = scheduler.spawn(filename, parameters.reverse);
                    runContext.callStack.push(rcid);
                    break;
                case SystemCalls.recv:
                    return InterpreterResult.recv;
                case SystemCalls.println:
                    auto s = runContext.popString();
                    writeln(s);
                    runContext.callStack.push(1);
                    break;
                case SystemCalls.display:
                    auto topValue = runContext.callStack.pop();
                    writeln("DISPLAY: " ~ to!string(topValue));
                    runContext.callStack.push(1);
                    break;
                default:
                    throw new InterpreterError("SYS is not implemented");
                }
                runContext.pc += 8;
                break;
            case Opcodes.and:
                runContext.callStack.op((operand1, operand2) =>
                                        (operand1 != 0) && (operand2 == 0) ? 1 : 0);
                break;
            case Opcodes.or:
                runContext.callStack.op((operand1, operand2) =>
                                        (operand1 != 0) || (operand2 != 0) ? 1 : 0);
                break;
            case Opcodes.not:
                auto operand = runContext.callStack.pop();
                runContext.callStack.push(!(operand != 0) ? 1 : 0);
                break;
            case Opcodes.eq:
                runContext.callStack.op((operand1, operand2) =>
                              (operand1 == operand2) ? 1 : 0);
                break;
            case Opcodes.neq:
                runContext.callStack.op((operand1, operand2) =>
                              (operand1 != operand2) ? 1 : 0);
                break;
            case Opcodes.lt:
                runContext.callStack.op((operand1, operand2) =>
                              operand1 < operand2 ? 1 : 0);
                break;
            case Opcodes.gt:
                runContext.callStack.op((operand1, operand2) =>
                              operand1 > operand2 ? 1 : 0);
                break;
            case Opcodes.nop:
                break;
            case Opcodes.halt:
                return InterpreterResult.halt;
            default:
                throw new InterpreterError(
                              "Invalid opcode" ~
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
