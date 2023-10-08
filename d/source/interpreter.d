module interpreter;

import std.conv : to;
import std.datetime : Duration, Clock;
import std.stdio : write, writeln;
import std.algorithm.iteration : map;
import std.array;

import std.range : iota;
import program;
import fiber;
import scheduler;

class InterpreterError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
}

enum InterpreterResult {
    halt,
    timeout
}

struct Interpreter {
    InterpreterResult run(ref Scheduler scheduler, ref Fiber fiber,
                          Duration timeSlice, uint timeoutGranularity) {
        auto startTime = Clock.currTime();
        uint instructionsExecuted = 0;
        InterpreterResult interpreterResult;

        while (true) {
            auto byteCode = fiber.program.byteCode;

            debug(interpreter) {
                write(to!string(fiber.fid) ~ ": ");
                writeln(fiber.stack);
                write(to!string(fiber.fid) ~ ": ");
                fiber.program.prettyPrint(&byteCode[fiber.PC], true);
            }

            //Thread.sleep(dur!"msecs"(50));
            //readln();

            if (++fiber.PC > byteCode.length) {
                throw new InterpreterError(
                              "Unexpected end of bytecode or invalid jump");
            }

            switch (byteCode[fiber.PC - 1] >> 3) {
            case Opcodes.PUSH:
                auto value = get!long(&byteCode[fiber.PC]);
                fiber.push(value);
                fiber.PC += 8;
                break;
            case Opcodes.PUSHS:
                auto result = fiber.pushData(byteCode[fiber.PC .. $]);
                auto dataAddress = result[0];
                auto length = result[1];
                fiber.push(dataAddress);
                fiber.PC += 4 + length;
                break;
            case Opcodes.POP:
                fiber.pop();
                break;
            case Opcodes.DUP:
                fiber.dup();
                break;
            case Opcodes.SWAP:
                fiber.swap();
                break;
            case Opcodes.LOAD:
                auto register =
                    cast(ubyte)(byteCode[fiber.PC - 1] & 0b00000111);
                fiber.load(register);
                break;
            case Opcodes.STORE:
                auto register =
                    cast(ubyte)(byteCode[fiber.PC - 1] & 0b00000111);
                fiber.store(register);
                break;
            case Opcodes.ADD:
                fiber.op((operand1, operand2) => operand1 + operand2);
                break;
            case Opcodes.SUB:
                fiber.op((operand1, operand2) => operand1 - operand2);
                break;
            case Opcodes.MUL:
                fiber.op((operand1, operand2) => operand1 * operand2);
                break;
            case Opcodes.DIV:
                fiber.op((operand1, operand2) => operand1 / operand2);
                break;
            case Opcodes.JUMP:
                auto byteIndex = get!long(&byteCode[fiber.PC]);
                fiber.PC = byteIndex;
                break;
            case Opcodes.CJUMP:
                auto byteIndex = get!long(&byteCode[fiber.PC]);
                auto conditional = fiber.pop();
                if (conditional != 0) {
                    fiber.PC = byteIndex;
                } else {
                    fiber.PC += 8;
                }
                break;
            case Opcodes.CALL:
                // Extract call operands
                int byteIndex = get!int(&byteCode[fiber.PC]);
                int arity = get!int(&byteCode[fiber.PC + 4]);
                // Add return address to stack
                fiber.push(fiber.PC + 8);
                // Save previous FP on the stack
                fiber.push(fiber.FP);
                // Set FP to first parameter CALL parameter
                fiber.FP = fiber.stack.length - 2 - arity;
                // Jump to CALL byte index
                fiber.PC = byteIndex;
                // Save previous data FP on the data stack
                insert(fiber.DATA_FP, fiber.dataStack);
                // Set data FP to the previous data FP
                fiber.DATA_FP = fiber.dataStack.length - 8;
                break;
            case Opcodes.RET:
                // Is the return done by value or by copy?
                auto returnMode =
                    cast(ubyte)(byteCode[fiber.PC - 1] & 0b00000111);
                // Swap return value and previous FP
                fiber.swap();
                // Restore FP to previous FP
                auto currentFP = fiber.FP;
                fiber.FP = fiber.pop();
                // Swap return value and return address
                fiber.swap();
                // Pop return address
                auto returnAddress = fiber.pop();
                // Has stack been exhausted?
                if (fiber.FP == -1 || returnAddress == 0) {
                    return InterpreterResult.halt;
                }
                // Pop return value
                auto returnValue = fiber.pop();
                ubyte[] returnData = null;
                if (returnMode == ReturnModes.COPY) {
                    // Extract return data
                    returnData = fiber.peekData(returnValue);
                }
                // Remove call stack frame
                fiber.stack = fiber.stack[0 .. currentFP];
                // Jump to return address
                fiber.PC = returnAddress;
                // Remove data stack frame
                auto previousDFP =
                    get!long(&fiber.dataStack[fiber.DATA_FP]);
                fiber.dataStack = fiber.dataStack[0 .. fiber.DATA_FP];
                // Restore data FP to previous data FP
                fiber.DATA_FP = previousDFP;
                // Reinsert return value on call stack (and data stack)
                if (returnMode == ReturnModes.COPY) {
                    // Copy the return data onto the caller's data stack
                    auto result = fiber.pushData(returnData);
                    auto dataAddress = result[0];
                    // Push the return data address onto caller's call stack
                    fiber.push(dataAddress);
                } else {
                    // Push the return value onto caller's call stack
                    fiber.push(returnValue);
                }
                break;
            case Opcodes.SYS:
                auto systemCall = get!long(&byteCode[fiber.PC]);
                switch (systemCall) {
                case SystemCalls.SPAWN:
                    long n = fiber.pop();
                    long[] parameters = iota(n).map!(_ => fiber.pop()).array;
                    string filename = fiber.popString();
                    long fid = scheduler.spawn(filename, parameters);
                    fiber.push(fid);
                    break;
                case SystemCalls.PRINTLN:
                    string s = fiber.popString();
                    writeln("PRINTLN: " ~ s);
                    fiber.push(1);
                    break;
                default:
                    throw new InterpreterError("SYS is not implemented");
                }
                fiber.PC += 8;
                break;
            case Opcodes.AND:
                fiber.op((operand1, operand2) =>
                          (operand1 != 0) && (operand2 == 0) ? 1 : 0);
                break;
            case Opcodes.OR:
                fiber.op((operand1, operand2) =>
                         (operand1 != 0) || (operand2 != 0) ? 1 : 0);
                break;
            case Opcodes.NOT:
                auto operand = fiber.pop();
                fiber.push(!(operand != 0) ? 1 : 0);
                break;
            case Opcodes.EQ:
                fiber.op((operand1, operand2) => (operand1 == operand2) ? 1 : 0);
                break;
            case Opcodes.NEQ:
                fiber.op((operand1, operand2) => (operand1 != operand2) ? 1 : 0);
                break;
            case Opcodes.LT:
                fiber.op((operand1, operand2) => operand1 < operand2 ? 1 : 0);
                break;
            case Opcodes.GT:
                fiber.op((operand1, operand2) => operand1 > operand2 ? 1 : 0);
                break;
            case Opcodes.NOP:
                break;
            case Opcodes.HALT:
                return InterpreterResult.halt;
            default:
                throw new InterpreterError(
                              "Invalid opcode" ~
                              to!string(byteCode[fiber.PC - 1] >> 3));
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
