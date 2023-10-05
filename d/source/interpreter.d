module interpreter;

import std.conv : to;
import std.datetime : Duration, Clock;
import std.stdio : writeln;
import std.range : iota;
import std.algorithm : each;
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
            case PUSHR:
                auto register = cast(ubyte)(byte_code[fiber.pc] & 0b00000111);
                if (register == SP) {
                    push(fiber.stack.length - 1, fiber);
                } else if (register == FP) {
                    push(fiber.fp, fiber);
                } else { // Must be PC
                    push(fiber.pc, fiber);
                }
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
            case LOADR:
                auto register = cast(ubyte)(byte_code[fiber.pc] & 0b00000111);
                loadr(register, fiber);
                break;
            case STORER:
                auto register = cast(ubyte)(byte_code[fiber.pc] & 0b00000111);
                storer(register, fiber);
                break;
            case MOVER:
                auto register = cast(ubyte)(byte_code[fiber.pc] & 0b00000111);
                pc_updated = mover(register, fiber);
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
                auto byte_index = get_ulong(&byte_code[fiber.pc + 1]);
                fiber.pc = byte_index;
                pc_updated = true;
                break;
            case CJUMP:
                auto byte_index = get_ulong(&byte_code[fiber.pc + 1]);
                auto conditional = pop(fiber);
                if (conditional != 0) {
                    fiber.pc = byte_index;
                } else {
                    fiber.pc += 8 + 1;
                }
                pc_updated = true;
                break;
            case CALL:
                auto byte_index = get_ulong(&byte_code[fiber.pc + 1]);
                push(fiber.pc + 1 + 8, fiber); // Add return address to stack
                fiber.pc = byte_index;
                pc_updated = true;
                push(fiber.fp, fiber); // Save previous FP on the stack
                fiber.fp = fiber.stack.length - 1; // Sets FP to SP
                break;
            case RET:
                if (fiber.stack.length == 1) {
                    // The stack is exhausted!
                    return InterpreterResult.halt;
                }
                swap(fiber);
                mover(FP, fiber); // Restores FP to saved FP
                swap(fiber);
                auto return_address = pop(fiber);
                auto return_value = pop(fiber);
                auto number_of_parameters = pop(fiber);
                pop(number_of_parameters, fiber);
                push(return_value, fiber);
                fiber.pc = return_address;
                pc_updated = true;
                break;
            case SYS:
                auto sys_name = get_ulong(&byte_code[fiber.pc + 1]);
                switch (sys_name) {
                    // WORK IN PROGRESS!!!!!!!!!!!!!
                case SYS_SPAWN:
                    // auto file_index = fiber.stack[$ - 1];
                    auto number_of_parameters = fiber.stack[$ - 2];
                    // Kludge!
                    string filename = "foo.txt";
                    ulong fib = scheduler.spawn(filename,
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

    private void pop(long n, ref Fiber fiber) {
        iota(n).each!(_ => pop(fiber));
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

    private void loadr(ubyte register, ref Fiber fiber) {
        auto offset = pop(fiber);
        if (register == SP) {
            push(fiber.stack[$ - 1 - offset], fiber);
        } else { // Must be FP
            push(fiber.stack[fiber.fp - offset], fiber);
        }
    }

    private void storer(ubyte register, ref Fiber fiber) {
        auto offset = pop(fiber);
        auto new_value = pop(fiber);
        if (register == SP) {
            fiber.stack[$ - 1 - offset] = new_value;
        } else { // Must be FP
            fiber.stack[fiber.fp - offset] = new_value;
        }
    }

    private bool mover(ubyte register, ref Fiber fiber) {
        auto topValue = pop(fiber);
        if (register == SP) {
            pop(fiber.stack.length - 1 - topValue, fiber);
            return false;
        } else if (register == FP) {
            fiber.fp = topValue;
            return false;
        } else { // Must be PC
            fiber.pc = topValue;
            return true;
        }
    }

    private void apply(long delegate(long, long) op, ref Fiber fiber) {
        auto operand2 = pop(fiber);
        auto operand1 = pop(fiber);
        push(op(operand1, operand2), fiber);
    }
}
