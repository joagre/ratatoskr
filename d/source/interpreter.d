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
                          Duration time_slice, uint timeout_granularity) {
        auto start_time = Clock.currTime();
        uint instructions_executed = 0;
        InterpreterResult interpreterResult;

        while (true) {
            auto byte_code = fiber.program.byte_code;

            debug(interpreter) {
                writeln(fiber.stack);
                fiber.program.pretty_print(&byte_code[fiber.PC], true);
            }

            //Thread.sleep(dur!"msecs"(50));
            //readln();

            if (++fiber.PC > byte_code.length) {
                throw new InterpreterError(
                                           "Unexpected end of bytecode or invalid jump");
            }

            switch (byte_code[fiber.PC - 1] >> 3) {
            case PUSH:
                auto value = get!long(&byte_code[fiber.PC]);
                push(value, fiber);
                fiber.PC += 8;
                break;
            case PUSHS:
                auto result = dpush(byte_code[fiber.PC .. $], fiber);
                auto data_address = result[0];
                auto length = result[1];
                push(data_address, fiber);
                fiber.PC += 4 + length;
                break;
            case POP:
                pop(fiber);
                break;
            case DUP:
                dup(fiber);
                break;
            case SWAP:
                swap(fiber);
                break;
            case LOAD:
                auto register =
                    cast(ubyte)(byte_code[fiber.PC - 1] & 0b00000111);
                load(register, fiber);
                break;
            case STORE:
                auto register =
                    cast(ubyte)(byte_code[fiber.PC - 1] & 0b00000111);
                store(register, fiber);
                break;
            case ADD:
                apply((operand1, operand2) => operand1 + operand2, fiber);
                break;
            case SUB:
                apply((operand1, operand2) => operand1 - operand2, fiber);
                break;
            case MUL:
                apply((operand1, operand2) => operand1 * operand2, fiber);
                break;
            case DIV:
                apply((operand1, operand2) => operand1 / operand2, fiber);
                break;
            case JUMP:
                auto byte_index = get!long(&byte_code[fiber.PC]);
                fiber.PC = byte_index;
                break;
            case CJUMP:
                auto byte_index = get!long(&byte_code[fiber.PC]);
                auto conditional = pop(fiber);
                if (conditional != 0) {
                    fiber.PC = byte_index;
                } else {
                    fiber.PC += 8;
                }
                break;
            case CALL:
                // Extract call operands
                int byte_index = get!int(&byte_code[fiber.PC]);
                int arity = get!int(&byte_code[fiber.PC + 4]);
                // Add return address to stack
                push(fiber.PC + 8, fiber);
                // Save previous FP on the stack
                push(fiber.FP, fiber);
                // Set FP to first parameter CALL parameter
                fiber.FP = fiber.stack.length - 2 - arity;
                // Jump to CALL byte index
                fiber.PC = byte_index;
                // Save previous data FP on the data stack
                insert(fiber.DFP, fiber.data_stack);
                // Set data FP to the previous data FP
                fiber.DFP = fiber.data_stack.length - 8;
                break;
            case RET:
                // Is the return done by value or by copy?
                auto return_mode =
                    cast(ubyte)(byte_code[fiber.PC - 1] & 0b00000111);
                // Swap return value and previous FP
                swap(fiber);
                // Restore FP to previous FP
                auto current_fp = fiber.FP;
                fiber.FP = pop(fiber);
                // Swap return value and return address
                swap(fiber);
                // Pop return address
                auto return_address = pop(fiber);
                // Has stack been exhausted?
                if (fiber.FP == -1 || return_address == 0) {
                    return InterpreterResult.halt;
                }
                // Pop return value
                auto return_value = pop(fiber);
                ubyte[] return_data = null;
                if (return_mode == RETURN_COPY) {
                    // Extract return data
                    return_data = dpeek(return_value, fiber);
                }
                // Remove call stack frame
                fiber.stack = fiber.stack[0 .. current_fp];
                // Jump to return address
                fiber.PC = return_address;
                // Remove data stack frame
                auto previous_dfp =
                    get!long(&fiber.data_stack[fiber.DFP]);
                fiber.data_stack = fiber.data_stack[0 .. fiber.DFP];
                // Restore data FP to previous data FP
                fiber.DFP = previous_dfp;
                // Reinsert return value on call stack (and data stack)
                if (return_mode == RETURN_COPY) {
                    // Copy the return data on to the caller's data stack
                    auto result = dpush(return_data, fiber);
                    auto data_address = result[0];
                    // Push the return data address onto caller's call stack
                    push(data_address, fiber);
                } else {
                    // Push the return value onto caller's call stack
                    push(return_value, fiber);
                }
                break;
            case SYS:
                auto sys_name = get!long(&byte_code[fiber.PC]);
                switch (sys_name) {
                    /*
                    // WORK IN PROGRESS!!!!!!!!!!!!!
                    case SYS_SPAWN:
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
                case SYS_PRINTLN:
                    long data_address = pop(fiber);
                    auto bytes = dpeek(data_address, fiber);
                    writeln("PRINTLN: " ~ cast(char[])bytes[4 .. $]);
                    push(1, fiber);
                    break;
                default:
                    throw new InterpreterError("SYS is not implemented");
                }
                fiber.PC += 8;
                break;
            case AND:
                apply((operand1, operand2) =>
                          (operand1 != 0) && (operand2 == 0) ? 1 : 0,
                      fiber);
                break;
            case OR:
                apply((operand1, operand2) =>
                          (operand1 != 0) || (operand2 != 0) ? 1 : 0,
                      fiber);
                break;
            case NOT:
                auto operand = pop(fiber);
                push(!(operand != 0) ? 1 : 0, fiber);
                break;
            case EQ:
                apply((operand1, operand2) =>
                          (operand1 == operand2) ? 1 : 0,
                      fiber);
                break;
            case NEQ:
                apply((operand1, operand2) =>
                          (operand1 != operand2) ? 1 : 0,
                      fiber);
                break;
            case LT:
                apply((operand1, operand2) =>
                          operand1 < operand2 ? 1 : 0,
                      fiber);
                break;
            case GT:
                apply((operand1, operand2) =>
                          operand1 > operand2 ? 1 : 0,
                      fiber);
                break;
            case NOP:
                break;
            case HALT:
                return InterpreterResult.halt;
            default:
                throw new InterpreterError(
                              "Invalid opcode" ~
                              to!string(byte_code[fiber.PC - 1] >> 3));
            }

            if (instructions_executed ++ >= timeout_granularity) {
                if (Clock.currTime() - start_time >= time_slice) {
                    interpreterResult = InterpreterResult.timeout;
                    break;
                }
                instructions_executed = 0;
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
        if (register == SP) {
            push(fiber.stack[$ - 1 - offset], fiber);
        } else { // Must be FP
            push(fiber.stack[fiber.FP - offset], fiber);
        }
    }

    private void store(ubyte register, ref Fiber fiber) {
        auto offset = pop(fiber);
        auto new_value = pop(fiber);
        if (register == SP) {
            fiber.stack[$ - 1 - offset] = new_value;
        } else { // Must be FP
            fiber.stack[fiber.FP - offset] = new_value;
        }
    }

    private void apply(long delegate(long, long) op, ref Fiber fiber) {
        auto operand2 = pop(fiber);
        auto operand1 = pop(fiber);
        push(op(operand1, operand2), fiber);
    }

    private Tuple!(long, int) dpush(ubyte[] bytes, ref Fiber fiber) {
        auto length = get!int(&bytes[0]);
        auto data_address = fiber.data_stack.length;
        fiber.data_stack ~= bytes[0 .. 4 + length + 1];
        return Tuple!(long, int)(data_address, length);
    }

    private ubyte[] dpeek(long data_address, ref Fiber fiber) {
        ubyte[] bytes = fiber.data_stack[data_address .. $];
        auto length = get!int(&bytes[0]);
        return bytes[0 .. 4 + length + 1];
    }
}
