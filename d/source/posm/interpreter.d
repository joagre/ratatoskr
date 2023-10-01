module interpreter;

import std.conv;
import core.time;
import std.datetime;
import std.stdio;
import core.thread;
import program;
import runcontext;

class InterpreterError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
}

enum RunResult {
    halt,
    timeout
}

struct Interpreter {
    RunResult run(ref RunContext run_context, const Duration timeSlice,
                  const uint instructionsPerCheck) {
        auto startTime = Clock.currTime();
        uint instructionsExecuted = 0;
        RunResult runResult;

        while (true) {
            auto pc = run_context.pc;
            Instruction instruction = run_context.program.instructions[pc];

            debug {
                writeln(run_context.stack);
                run_context.program.pretty_print(pc, instruction);
            }

            //Thread.sleep(dur!"msecs"(500));
            //readln();

            switch (instruction.opcode) {
            case Opcode.PUSH:
                run_context.stack ~= instruction.operand;
                break;
            case Opcode.PUSHR:
                if (instruction.operand == SP) {
                    run_context.stack ~= run_context.stack.length - 1;
                } else if (instruction.operand == FP) {
                    run_context.stack ~= run_context.fp;
                } else {
                    run_context.stack ~= run_context.pc;
                }
                break;
            case Opcode.POP:
                run_context.stack = run_context.stack[0 .. $ - 1];
                break;
            case Opcode.DUP:
                auto topValue = run_context.stack[$ - 1];
                run_context.stack ~= topValue;
                break;
            case Opcode.SWAP:
                auto topValue = run_context.stack[$ - 1];
                run_context.stack[$ - 1] = run_context.stack[$ - 2];
                run_context.stack[$ - 2] = topValue;
                break;
            case Opcode.LOADR:
                auto offset = run_context.stack[$ - 1];
                if (instruction.operand == SP) {
                    run_context.stack[$ - 1] =
                        run_context.stack[$ - 1 - offset];
                } else { // Must be FP
                    run_context.stack[$ - 1] =
                        run_context.stack[run_context.fp - offset];
                }
                break;
            case Opcode.STORER:
                auto offset = run_context.stack[$ - 1];
                auto new_value = run_context.stack[$ - 2];
                if (instruction.operand == SP) {
                    // Do not count the STORER parameters
                    run_context.stack[$ - 1 - 2 - offset] = new_value;
                } else { // Must be FP
                    run_context.stack[run_context.fp - offset] = new_value;
                }
                run_context.stack = run_context.stack[0 .. $ - 2];
                break;
            case Opcode.MOVER:
                auto topValue = run_context.stack[$ - 1];
                run_context.stack = run_context.stack[0 .. $ - 1];
                if (instruction.operand == SP) {
                    run_context.stack = run_context.stack[0 .. topValue + 1];
                } else if (instruction.operand == FP) {
                    run_context.fp = topValue;
                } else { // Must be PC
                    run_context.pc = topValue;
                }
                break;
            case Opcode.ADD:
                auto operand2 = run_context.stack[$ - 1];
                auto operand1 = run_context.stack[$ - 2];
                run_context.stack = run_context.stack[0 .. $ - 2];
                run_context.stack ~= operand1 + operand2;
                break;
            case Opcode.SUB:
                auto operand2 = run_context.stack[$ - 1];
                auto operand1 = run_context.stack[$ - 2];
                run_context.stack = run_context.stack[0 .. $ - 2];
                run_context.stack ~= operand1 - operand2;
                break;
            case Opcode.MUL:
                auto operand2 = run_context.stack[$ - 1];
                auto operand1 = run_context.stack[$ - 2];
                run_context.stack = run_context.stack[0 .. $ - 2];
                run_context.stack ~= operand1 * operand2;
                break;
            case Opcode.DIV:
                auto operand2 = run_context.stack[$ - 1];
                auto operand1 = run_context.stack[$ - 2];
                run_context.stack = run_context.stack[0 .. $ - 2];
                run_context.stack ~= operand1 / operand2;
                break;
            case Opcode.JUMP:
                run_context.pc = instruction.operand;
                break;
            case Opcode.CJUMP:
                auto conditional = run_context.stack[$ - 1];
                run_context.stack = run_context.stack[0 .. $ - 1];
                if (conditional != 0) {
                    run_context.pc = instruction.operand;
                }
                break;
            case Opcode.CALL:
                auto address = run_context.stack[$ - 1];
                run_context.stack ~= run_context.pc + 1;
                run_context.pc = instruction.operand;
                break;
            case Opcode.RET:
                auto return_address = run_context.stack[$ - 1];
                auto result = run_context.stack[$ - 2];
                auto number_of_parameters = run_context.stack[$ - 3];
                run_context.stack =
                    run_context.stack[0 .. $ - 3 - number_of_parameters];
                run_context.stack ~= result;
                run_context.pc = return_address;
                break;
            case Opcode.SYS:
                throw new InterpreterError("SYS is not implemented");
            case Opcode.AND:
                auto operand2 = run_context.stack[$ - 1];
                auto operand1 = run_context.stack[$ - 2];
                run_context.stack = run_context.stack[0 .. $ - 2];
                run_context.stack ~= (operand1 != 0) && (operand2 == 0);
                break;
            case Opcode.OR:
                auto operand2 = run_context.stack[$ - 1];
                auto operand1 = run_context.stack[$ - 2];
                run_context.stack = run_context.stack[0 .. $ - 2];
                run_context.stack ~= (operand1 != 0) || (operand2 != 0);
                break;
            case Opcode.NOT:
                auto operand = run_context.stack[$ - 1];
                run_context.stack = run_context.stack[0 .. $ - 1];
                run_context.stack ~= !(operand != 0);
                break;
            case Opcode.EQ:
                auto operand2 = run_context.stack[$ - 1];
                auto operand1 = run_context.stack[$ - 2];
                run_context.stack = run_context.stack[0 .. $ - 2];
                run_context.stack ~= operand1 == operand2;
                break;
            case Opcode.NEQ:
                auto operand2 = run_context.stack[$ - 1];
                auto operand1 = run_context.stack[$ - 2];
                run_context.stack = run_context.stack[0 .. $ - 2];
                run_context.stack ~= operand1 != operand2;
                break;
            case Opcode.LT:
                auto operand2 = run_context.stack[$ - 1];
                auto operand1 = run_context.stack[$ - 2];
                run_context.stack = run_context.stack[0 .. $ - 2];
                run_context.stack ~= operand1 < operand2;
                break;
            case Opcode.GT:
                auto operand2 = run_context.stack[$ - 1];
                auto operand1 = run_context.stack[$ - 2];
                run_context.stack = run_context.stack[0 .. $ - 2];
                run_context.stack ~= operand1 > operand2;
                break;
            case Opcode.NOP:
                break;
            case Opcode.HALT:
                return RunResult.halt;
            default:
                throw new InterpreterError("Invalid opcode");
            }

            if (instructionsExecuted++ >= instructionsPerCheck) {
                if (Clock.currTime() - startTime >= timeSlice) {
                    runResult = RunResult.timeout;
                    break;
                }
                instructionsExecuted = 0;
            }

            if (pc == run_context.pc) {
                run_context.pc++;
            }
        }

        return runResult;
    }
}
