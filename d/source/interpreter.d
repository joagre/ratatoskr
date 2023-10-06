module interpreter;

import std.conv : to;
import std.datetime : Duration, Clock;
import std.stdio : writeln;
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
        InterpreterResult result;

        while (true) {
            auto byte_code = fiber.program.byte_code;

            debug(interpreter) {
                writeln(fiber.stack);
                fiber.program.pretty_print(&byte_code[fiber.pc], true);
            }

            //Thread.sleep(dur!"msecs"(50));
            //readln();

            if (++fiber.pc >= byte_code.length) {
                throw new InterpreterError(
                              "Unexpected end of bytecode or invalid jump");
            }

            switch (byte_code[fiber.pc - 1] >> 3) {
            case PUSH:
                auto value = get!long(&byte_code[fiber.pc]);
                push(value, fiber);
                fiber.pc += 8;
                break;
            case PUSHS:
                auto length = get!int(&byte_code[fiber.pc]);
                auto next_data_address = fiber.data_stack.length;
                fiber.data_stack ~=
                    byte_code[fiber.pc .. fiber.pc + 4 + length + 1];
                push(next_data_address, fiber);
                fiber.pc += 4 + length;
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
                    cast(ubyte)(byte_code[fiber.pc - 1] & 0b00000111);
                load(register, fiber);
                break;
            case STORE:
                auto register =
                    cast(ubyte)(byte_code[fiber.pc - 1] & 0b00000111);
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
                auto byte_index = get!long(&byte_code[fiber.pc]);
                fiber.pc = byte_index;
                break;
            case CJUMP:
                auto byte_index = get!long(&byte_code[fiber.pc]);
                auto conditional = pop(fiber);
                if (conditional != 0) {
                    fiber.pc = byte_index;
                } else {
                    fiber.pc += 8;
                }
                break;
            case CALL:
                // Extract call operands
                int byte_index = get!int(&byte_code[fiber.pc]);
                int arity = get!int(&byte_code[fiber.pc + 4]);
                // Add return address to stack
                push(fiber.pc + 8, fiber);
                // Save previous FP on the stack
                push(fiber.fp, fiber);
                // Set FP to first parameter CALL parameter
                fiber.fp = fiber.stack.length - 2 - arity;
                // Jump to CALL byte index
                fiber.pc = byte_index;
                // Save previous data FP on the data stack
                insert(fiber.data_fp, fiber.data_stack);
                // Set data FP to the previous data FP
                fiber.data_fp = fiber.data_stack.length - 8;
                break;
            case RET:
                auto current_fp = fiber.fp;
                // Swap return value and previous FP
                swap(fiber);
                // Restore FP to previous FP
                fiber.fp = pop(fiber);
                if (fiber.fp == -1) {
                    // Swap the return value and the return address
                    swap(fiber);
                    // Pop the return address
                    pop(fiber);
                    return InterpreterResult.halt;
                }
                // Swap the return value and the return address
                swap(fiber);
                auto return_address = pop(fiber);
                auto return_value = pop(fiber);
                // Remove the stack frame
                fiber.stack = fiber.stack[0 .. current_fp];
                // Reinsert return value
                push(return_value, fiber);
                // Jump to return address
                fiber.pc = return_address;
                // Remove the data stack frame
                auto previous_data_fp =
                    get!long(&fiber.data_stack[fiber.data_fp]);
                fiber.data_stack = fiber.data_stack[0 .. fiber.data_fp];
                fiber.data_fp = previous_data_fp;
                break;
            case SYS:
                auto sys_name = get!long(&byte_code[fiber.pc]);
                switch (sys_name) {
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
                default:
                    throw new InterpreterError("SYS is not implemented");
                }
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
                              to!string(byte_code[fiber.pc - 1] >> 3));
            }

            if (instructions_executed ++ >= timeout_granularity) {
                if (Clock.currTime() - start_time >= time_slice) {
                    result = InterpreterResult.timeout;
                    break;
                }
                instructions_executed = 0;
            }
        }

        return result;
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
            push(fiber.stack[fiber.fp - offset], fiber);
        }
    }

    private void store(ubyte register, ref Fiber fiber) {
        auto offset = pop(fiber);
        auto new_value = pop(fiber);
        if (register == SP) {
            fiber.stack[$ - 1 - offset] = new_value;
        } else { // Must be FP
            fiber.stack[fiber.fp - offset] = new_value;
        }
    }

    private void apply(long delegate(long, long) op, ref Fiber fiber) {
        auto operand2 = pop(fiber);
        auto operand1 = pop(fiber);
        push(op(operand1, operand2), fiber);
    }
}
