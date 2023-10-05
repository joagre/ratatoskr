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

            bool pc_updated = false;
            switch (byte_code[fiber.pc] >> 3) {
            case PUSH:
                auto value = get_long(&byte_code[fiber.pc + 1]);
                push(value, fiber);
                fiber.pc += 8 + 1;
                pc_updated = true;
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
                auto register = cast(ubyte)(byte_code[fiber.pc] & 0b00000111);
                load(register, fiber);
                break;
            case STORE:
                auto register = cast(ubyte)(byte_code[fiber.pc] & 0b00000111);
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
                auto byte_index = get_long(&byte_code[fiber.pc + 1]);
                fiber.pc = byte_index;
                pc_updated = true;
                break;
            case CJUMP:
                auto byte_index = get_long(&byte_code[fiber.pc + 1]);
                auto conditional = pop(fiber);
                if (conditional != 0) {
                    fiber.pc = byte_index;
                } else {
                    fiber.pc += 8 + 1;
                }
                pc_updated = true;
                break;
            case CALL:
                //
                // Call stack management
                //

                int byte_index = get_int(&byte_code[fiber.pc + 1]);
                int arity = get_int(&byte_code[fiber.pc + 5]);
                // Adds return address to stack
                push(fiber.pc + 1 + 8, fiber);
                // Saves previous FP on the stack
                push(fiber.fp, fiber);
                // Sets FP to first parameter
                fiber.fp = fiber.stack.length - 2 - arity;
                // Jump to CALL label
                fiber.pc = byte_index;
                pc_updated = true;

                //
                // Data stack management
                //

                // Saves previous Data FP on the data stack
                insert_long(fiber.data_fp, fiber.data_stack);
                fiber.data_fp = fiber.data_stack.length - 8;

                break;
            case RET:
                if (fiber.stack.length == 1) {
                    // The stack is exhausted!
                    return InterpreterResult.halt;
                }

                //
                // Call stack management
                //

                auto current_fp = fiber.fp;
                swap(fiber);
                // Restores FP to saved FP
                fiber.fp = pop(fiber);
                swap(fiber);
                auto return_address = pop(fiber);
                auto return_value = pop(fiber);
                // Remove stack frame
                fiber.stack = fiber.stack[0 .. current_fp];
                // Reinserts return value
                push(return_value, fiber);
                // Jumps to return address
                fiber.pc = return_address;
                pc_updated = true;

                //
                // Data stack management
                //

                auto previous_data_fp =
                    get_long(&fiber.data_stack[fiber.data_fp]);
                fiber.data_stack = fiber.data_stack[0 .. fiber.data_fp];
                fiber.data_fp = previous_data_fp;

                break;
            case SYS:
                auto sys_name = get_long(&byte_code[fiber.pc + 1]);
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
                              to!string(byte_code[fiber.pc] >> 3));
            }

            if (!pc_updated) {
                fiber.pc++;
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
