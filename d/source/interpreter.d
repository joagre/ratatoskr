module posm.interpreter;

import std.conv;
import core.time;
import std.datetime;
import std.stdio;
import posm.program;
import scheduler.fiber;

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
    InterpreterResult run(ref Fiber fiber, Duration time_slice,
                          uint timeout_granularity) {
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
                auto value = fiber.program.get_ulong(
                                 &byte_code[fiber.pc + 1]);
                fiber.stack ~= value;
                fiber.pc += 8 + 1;
                pc_updated = true;
                break;
            case PUSHR:
                auto register = byte_code[fiber.pc] & 0b00000111;
                if (register == SP) {
                    fiber.stack ~= fiber.stack.length - 1;
                } else if (register == FP) {
                    fiber.stack ~= fiber.fp;
                } else { // Must be PC
                    fiber.stack ~= fiber.pc;
                }
                break;
            case POP:
                fiber.stack = fiber.stack[0 .. $ - 1];
                break;
            case DUP:
                auto topValue = fiber.stack[$ - 1];
                fiber.stack ~= topValue;
                break;
            case SWAP:
                auto topValue = fiber.stack[$ - 1];
                fiber.stack[$ - 1] = fiber.stack[$ - 2];
                fiber.stack[$ - 2] = topValue;
                break;
            case LOADR:
                auto register = byte_code[fiber.pc] & 0b00000111;
                auto offset = fiber.stack[$ - 1];
                if (register == SP) {
                    fiber.stack[$ - 1] =
                        fiber.stack[$ - 1 - offset];
                } else { // Must be FP
                    fiber.stack[$ - 1] =
                        fiber.stack[fiber.fp - offset];
                }
                break;
            case STORER:
                auto register = byte_code[fiber.pc] & 0b00000111;
                auto offset = fiber.stack[$ - 1];
                auto new_value = fiber.stack[$ - 2];
                if (register == SP) {
                    // Do not count the STORER parameters
                    fiber.stack[$ - 1 - 2 - offset] = new_value;
                } else { // Must be FP
                    fiber.stack[fiber.fp - offset] = new_value;
                }
                fiber.stack = fiber.stack[0 .. $ - 2];
                break;
            case MOVER:
                auto register = byte_code[fiber.pc] & 0b00000111;
                auto topValue = fiber.stack[$ - 1];
                fiber.stack = fiber.stack[0 .. $ - 1];
                if (register == SP) {
                    fiber.stack = fiber.stack[0 .. topValue + 1];
                } else if (register == FP) {
                    fiber.fp = topValue;
                } else { // Must be PC
                    fiber.pc = topValue;
                    pc_updated = true;
                }
                break;
            case ADD:
                auto operand2 = fiber.stack[$ - 1];
                auto operand1 = fiber.stack[$ - 2];
                fiber.stack = fiber.stack[0 .. $ - 2];
                fiber.stack ~= operand1 + operand2;
                break;
            case SUB:
                auto operand2 = fiber.stack[$ - 1];
                auto operand1 = fiber.stack[$ - 2];
                fiber.stack = fiber.stack[0 .. $ - 2];
                fiber.stack ~= operand1 - operand2;
                break;
            case MUL:
                auto operand2 = fiber.stack[$ - 1];
                auto operand1 = fiber.stack[$ - 2];
                fiber.stack = fiber.stack[0 .. $ - 2];
                fiber.stack ~= operand1 * operand2;
                break;
            case DIV:
                auto operand2 = fiber.stack[$ - 1];
                auto operand1 = fiber.stack[$ - 2];
                fiber.stack = fiber.stack[0 .. $ - 2];
                fiber.stack ~= operand1 / operand2;
                break;
            case JUMP:
                auto byte_index =
                    fiber.program.get_ulong(
                        &byte_code[fiber.pc + 1]);
                fiber.pc = byte_index;
                pc_updated = true;
                break;
            case CJUMP:
                auto byte_index =
                    fiber.program.get_ulong(
                        &byte_code[fiber.pc + 1]);
                auto conditional = fiber.stack[$ - 1];
                fiber.stack = fiber.stack[0 .. $ - 1];
                if (conditional != 0) {
                    fiber.pc = byte_index;
                } else {
                    fiber.pc += 8 + 1;
                }
                pc_updated = true;
                break;
            case CALL:
                auto byte_index =
                    fiber.program.get_ulong(
                        &byte_code[fiber.pc + 1]);
                auto address = fiber.stack[$ - 1];
                fiber.stack ~= fiber.pc + 1 + 8;
                fiber.pc = byte_index;
                pc_updated = true;
                break;
            case RET:
                auto return_address = fiber.stack[$ - 1];
                auto return_value = fiber.stack[$ - 2];
                auto number_of_parameters = fiber.stack[$ - 3];
                fiber.stack =
                    fiber.stack[0 .. $ - 3 - number_of_parameters];
                fiber.stack ~= return_value;
                fiber.pc = return_address;
                pc_updated = true;
                break;
            case SYS:
                throw new InterpreterError("SYS is not implemented");
            case AND:
                auto operand2 = fiber.stack[$ - 1];
                auto operand1 = fiber.stack[$ - 2];
                fiber.stack = fiber.stack[0 .. $ - 2];
                fiber.stack ~= (operand1 != 0) && (operand2 == 0);
                break;
            case OR:
                auto operand2 = fiber.stack[$ - 1];
                auto operand1 = fiber.stack[$ - 2];
                fiber.stack = fiber.stack[0 .. $ - 2];
                fiber.stack ~= (operand1 != 0) || (operand2 != 0);
                break;
            case NOT:
                auto operand = fiber.stack[$ - 1];
                fiber.stack = fiber.stack[0 .. $ - 1];
                fiber.stack ~= !(operand != 0);
                break;
            case EQ:
                auto operand2 = fiber.stack[$ - 1];
                auto operand1 = fiber.stack[$ - 2];
                fiber.stack = fiber.stack[0 .. $ - 2];
                fiber.stack ~= operand1 == operand2;
                break;
            case NEQ:
                auto operand2 = fiber.stack[$ - 1];
                auto operand1 = fiber.stack[$ - 2];
                fiber.stack = fiber.stack[0 .. $ - 2];
                fiber.stack ~= operand1 != operand2;
                break;
            case LT:
                auto operand2 = fiber.stack[$ - 1];
                auto operand1 = fiber.stack[$ - 2];
                fiber.stack = fiber.stack[0 .. $ - 2];
                fiber.stack ~= operand1 < operand2;
                break;
            case GT:
                auto operand2 = fiber.stack[$ - 1];
                auto operand1 = fiber.stack[$ - 2];
                fiber.stack = fiber.stack[0 .. $ - 2];
                fiber.stack ~= operand1 > operand2;
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
}
