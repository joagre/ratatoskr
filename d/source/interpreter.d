module interpreter;

import std.conv : to;
import std.datetime : Duration, Clock;
import std.stdio : writeln;
import std.typecons : Tuple;
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
                writeln(fiber.stack);
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
                push(value, fiber);
                fiber.PC += 8;
                break;
            case Opcodes.PUSHS:
                auto result = dpush(byteCode[fiber.PC .. $], fiber);
                auto dataAddress = result[0];
                auto length = result[1];
                push(dataAddress, fiber);
                fiber.PC += 4 + length;
                break;
            case Opcodes.POP:
                pop(fiber);
                break;
            case Opcodes.DUP:
                dup(fiber);
                break;
            case Opcodes.SWAP:
                swap(fiber);
                break;
            case Opcodes.LOAD:
                auto register =
                    cast(ubyte)(byteCode[fiber.PC - 1] & 0b00000111);
                load(register, fiber);
                break;
            case Opcodes.STORE:
                auto register =
                    cast(ubyte)(byteCode[fiber.PC - 1] & 0b00000111);
                store(register, fiber);
                break;
            case Opcodes.ADD:
                apply((operand1, operand2) => operand1 + operand2, fiber);
                break;
            case Opcodes.SUB:
                apply((operand1, operand2) => operand1 - operand2, fiber);
                break;
            case Opcodes.MUL:
                apply((operand1, operand2) => operand1 * operand2, fiber);
                break;
            case Opcodes.DIV:
                apply((operand1, operand2) => operand1 / operand2, fiber);
                break;
            case Opcodes.JUMP:
                auto byteIndex = get!long(&byteCode[fiber.PC]);
                fiber.PC = byteIndex;
                break;
            case Opcodes.CJUMP:
                auto byteIndex = get!long(&byteCode[fiber.PC]);
                auto conditional = pop(fiber);
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
                push(fiber.PC + 8, fiber);
                // Save previous FP on the stack
                push(fiber.FP, fiber);
                // Set FP to first parameter CALL parameter
                fiber.FP = fiber.stack.length - 2 - arity;
                // Jump to CALL byte index
                fiber.PC = byteIndex;
                // Save previous data FP on the data stack
                insert(fiber.DFP, fiber.dataStack);
                // Set data FP to the previous data FP
                fiber.DFP = fiber.dataStack.length - 8;
                break;
            case Opcodes.RET:
                // Is the return done by value or by copy?
                auto returnMode =
                    cast(ubyte)(byteCode[fiber.PC - 1] & 0b00000111);
                // Swap return value and previous FP
                swap(fiber);
                // Restore FP to previous FP
                auto currentFP = fiber.FP;
                fiber.FP = pop(fiber);
                // Swap return value and return address
                swap(fiber);
                // Pop return address
                auto returnAddress = pop(fiber);
                // Has stack been exhausted?
                if (fiber.FP == -1 || returnAddress == 0) {
                    return InterpreterResult.halt;
                }
                // Pop return value
                auto returnValue = pop(fiber);
                ubyte[] returnData = null;
                if (returnMode == ReturnModes.COPY) {
                    // Extract return data
                    returnData = dpeek(returnValue, fiber);
                }
                // Remove call stack frame
                fiber.stack = fiber.stack[0 .. currentFP];
                // Jump to return address
                fiber.PC = returnAddress;
                // Remove data stack frame
                auto previousDFP =
                    get!long(&fiber.dataStack[fiber.DFP]);
                fiber.dataStack = fiber.dataStack[0 .. fiber.DFP];
                // Restore data FP to previous data FP
                fiber.DFP = previousDFP;
                // Reinsert return value on call stack (and data stack)
                if (returnMode == ReturnModes.COPY) {
                    // Copy the return data on to the caller's data stack
                    auto result = dpush(returnData, fiber);
                    auto dataAddress = result[0];
                    // Push the return data address onto caller's call stack
                    push(dataAddress, fiber);
                } else {
                    // Push the return value onto caller's call stack
                    push(returnValue, fiber);
                }
                break;
            case Opcodes.SYS:
                auto systemCall = get!long(&byteCode[fiber.PC]);
                switch (systemCall) {
                    /*
                    // WORK IN PROGRESS!!!!!!!!!!!!!
                    case SystemCalls.SPAWN:
                    // auto file_index = fiber.stack[$ - 1];
                    auto number_of_parameters = fiber.stack[$ - 2];
                    // Kludge!
                    string filename = "foo.txt";
                    long fib = scheduler.spawn(filename,
                                                fiber.stack[$ - 3 .. $]);
                    //pop(1 + number_of_parameters);
                    //push(fib);
                    break;
                    */
                case SystemCalls.PRINTLN:
                    long dataAddress = pop(fiber);
                    auto bytes = dpeek(dataAddress, fiber);
                    writeln("PRINTLN: " ~ cast(char[])bytes[4 .. $]);
                    push(1, fiber);
                    break;
                default:
                    throw new InterpreterError("SYS is not implemented");
                }
                fiber.PC += 8;
                break;
            case Opcodes.AND:
                apply((operand1, operand2) =>
                          (operand1 != 0) && (operand2 == 0) ? 1 : 0,
                      fiber);
                break;
            case Opcodes.OR:
                apply((operand1, operand2) =>
                          (operand1 != 0) || (operand2 != 0) ? 1 : 0,
                      fiber);
                break;
            case Opcodes.NOT:
                auto operand = pop(fiber);
                push(!(operand != 0) ? 1 : 0, fiber);
                break;
            case Opcodes.EQ:
                apply((operand1, operand2) =>
                          (operand1 == operand2) ? 1 : 0,
                      fiber);
                break;
            case Opcodes.NEQ:
                apply((operand1, operand2) =>
                          (operand1 != operand2) ? 1 : 0,
                      fiber);
                break;
            case Opcodes.LT:
                apply((operand1, operand2) =>
                          operand1 < operand2 ? 1 : 0,
                      fiber);
                break;
            case Opcodes.GT:
                apply((operand1, operand2) =>
                          operand1 > operand2 ? 1 : 0,
                      fiber);
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

    private void push(long value, ref Fiber fiber) {
        fiber.stack ~= value;
    }

    private long pop(ref Fiber fiber) {
        auto topValue = fiber.stack[$ - 1];
        fiber.stack = fiber.stack[0 .. $ - 1];
        return topValue;
    }

    private void dup(ref Fiber fiber) {
        auto topValue = fiber.stack[$ - 1];
        fiber.stack ~= topValue;
    }

    private void swap(ref Fiber fiber) {
        auto topValue = fiber.stack[$ - 1];
        fiber.stack[$ - 1] = fiber.stack[$ - 2];
        fiber.stack[$ - 2] = topValue;
    }

    private void load(ubyte register, ref Fiber fiber) {
        auto offset = pop(fiber);
        if (register == Registers.SP) {
            push(fiber.stack[$ - 1 - offset], fiber);
        } else { // Must be FP
            push(fiber.stack[fiber.FP - offset], fiber);
        }
    }

    private void store(ubyte register, ref Fiber fiber) {
        auto offset = pop(fiber);
        auto newValue = pop(fiber);
        if (register == Registers.SP) {
            fiber.stack[$ - 1 - offset] = newValue;
        } else { // Must be FP
            fiber.stack[fiber.FP - offset] = newValue;
        }
    }

    private void apply(long delegate(long, long) op, ref Fiber fiber) {
        auto operand2 = pop(fiber);
        auto operand1 = pop(fiber);
        push(op(operand1, operand2), fiber);
    }

    private Tuple!(long, int) dpush(ubyte[] bytes, ref Fiber fiber) {
        auto length = get!int(&bytes[0]);
        auto dataAddress = fiber.dataStack.length;
        fiber.dataStack ~= bytes[0 .. 4 + length + 1];
        return Tuple!(long, int)(dataAddress, length);
    }

    private ubyte[] dpeek(long dataAddress, ref Fiber fiber) {
        ubyte[] bytes = fiber.dataStack[dataAddress .. $];
        auto length = get!int(&bytes[0]);
        return bytes[0 .. 4 + length + 1];
    }
}
