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
                writeln(runContext.stack);
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
                runContext.push(value);
                runContext.pc += 8;
                break;
            case Opcodes.pushs:
                auto result = runContext.pushData(byteCode[runContext.pc .. $]);
                auto dataAddress = result[0];
                auto length = result[1];
                runContext.push(dataAddress);
                runContext.pc += 4 + length;
                break;
            case Opcodes.pop:
                runContext.pop();
                break;
            case Opcodes.dup:
                runContext.dup();
                break;
            case Opcodes.swap:
                runContext.swap();
                break;
            case Opcodes.load:
                auto register = cast(ubyte)(byteCode[currentPc] & 0b00000111);
                runContext.load(register);
                break;
            case Opcodes.store:
                auto register = cast(ubyte)(byteCode[currentPc] & 0b00000111);
                runContext.store(register);
                break;
            case Opcodes.add:
                runContext.op((operand1, operand2) => operand1 + operand2);
                break;
            case Opcodes.sub:
                runContext.op((operand1, operand2) => operand1 - operand2);
                break;
            case Opcodes.mul:
                runContext.op((operand1, operand2) => operand1 * operand2);
                break;
            case Opcodes.div:
                runContext.op((operand1, operand2) => operand1 / operand2);
                break;
            case Opcodes.jump:
                auto byteIndex = get!long(&byteCode[runContext.pc]);
                runContext.pc = byteIndex;
                break;
            case Opcodes.cjump:
                auto byteIndex = get!long(&byteCode[runContext.pc]);
                auto conditional = runContext.pop();
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
                runContext.push(runContext.pc + 8);
                // Save previous fp on the stack
                runContext.push(runContext.fp);
                // Set fp to first parameter CALL parameter
                runContext.fp = runContext.stack.length - 2 - arity;
                // Jump to CALL byte index
                runContext.pc = byteIndex;
                // Save previous data fp on the data stack
                insert(runContext.dataFp, runContext.dataStack);
                // Set data fp to the previous data fp
                runContext.dataFp = runContext.dataStack.length - 8;
                break;
            case Opcodes.ret:
                // Is the return done by value or by copy?
                auto returnMode = cast(ubyte)(byteCode[currentPc] & 0b00000111);
                // Swap return value and previous fp
                runContext.swap();
                // Restore fp to previous fp
                auto currentFp = runContext.fp;
                runContext.fp = runContext.pop();
                // Swap return value and return address
                runContext.swap();
                // Pop return address
                auto returnAddress = runContext.pop();
                // Has stack been exhausted?
                if (runContext.fp == -1 || returnAddress == 0) {
                    return InterpreterResult.halt;
                }
                // Pop return value
                auto returnValue = runContext.pop();
                ubyte[] returnData = null;
                if (returnMode == ReturnModes.copy) {
                    // Extract return data
                    returnData = runContext.peekData(returnValue);
                }
                // Remove call stack frame
                runContext.stack = runContext.stack[0 .. currentFp];
                // Jump to return address
                runContext.pc = returnAddress;
                // Remove data stack frame
                auto previousDataFp =
                    get!long(&runContext.dataStack[runContext.dataFp]);
                runContext.dataStack =
                    runContext.dataStack[0 .. runContext.dataFp];
                // Restore data fp to previous data fp
                runContext.dataFp = previousDataFp;
                // Reinsert return value on call stack (and data stack)
                if (returnMode == ReturnModes.copy) {
                    // Copy the return data onto the caller's data stack
                    auto result = runContext.pushData(returnData);
                    auto dataAddress = result[0];
                    // Push the return data address onto caller's call stack
                    runContext.push(dataAddress);
                } else {
                    // Push the return value onto caller's call stack
                    runContext.push(returnValue);
                }
                break;
            case Opcodes.sys:
                auto systemCall = get!long(&byteCode[runContext.pc]);
                switch (systemCall) {
                case SystemCalls.spawn:
                    auto n = runContext.pop();
                    auto parameters = iota(n).map!(_ => runContext.pop()).array;
                    auto filename = runContext.popString();
                    auto rcid = scheduler.spawn(filename, parameters.reverse);
                    runContext.push(rcid);
                    break;
                case SystemCalls.recv:
                    return InterpreterResult.recv;
                case SystemCalls.println:
                    auto s = runContext.popString();
                    writeln(s);
                    runContext.push(1);
                    break;
                case SystemCalls.display:
                    auto topValue = runContext.pop();
                    writeln("DISPLAY: " ~ to!string(topValue));
                    runContext.push(1);
                    break;
                default:
                    throw new InterpreterError("SYS is not implemented");
                }
                runContext.pc += 8;
                break;
            case Opcodes.and:
                runContext.op((operand1, operand2) =>
                          (operand1 != 0) && (operand2 == 0) ? 1 : 0);
                break;
            case Opcodes.or:
                runContext.op((operand1, operand2) =>
                         (operand1 != 0) || (operand2 != 0) ? 1 : 0);
                break;
            case Opcodes.not:
                auto operand = runContext.pop();
                runContext.push(!(operand != 0) ? 1 : 0);
                break;
            case Opcodes.eq:
                runContext.op((operand1, operand2) =>
                              (operand1 == operand2) ? 1 : 0);
                break;
            case Opcodes.neq:
                runContext.op((operand1, operand2) =>
                              (operand1 != operand2) ? 1 : 0);
                break;
            case Opcodes.lt:
                runContext.op((operand1, operand2) =>
                              operand1 < operand2 ? 1 : 0);
                break;
            case Opcodes.gt:
                runContext.op((operand1, operand2) =>
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
