#include <stdio.h>
#include <time.h>
#include "interpreter.h"
#include "call_stack.h"
#include "job.h"
#include "scheduler.h"
#include "loader.h"
#include "pretty_print.h"

void interpreter_init(interpreter_t *interpreter, loader_t* loader,
                      interpreter_mode_t mode) {
    interpreter->loader = loader;
    interpreter->mode = mode;
}

interpreter_result_t run(interpreter_t *interpreter, scheduler_t *scheduler,
                         job_t* job, uint32_t time_slice,
                         uint16_t check_after) {
    clock_t start_time = START_TIMER();
    uint32_t instructions_executed = 0;
    interpreter_result_t interpreter_result;

    while (true) {
#ifdef DEBUG
#ifndef MUTE_LOG_DEBUG
        if (interpreter->mode == INTERPRETER_MODE_REGISTER) {
            fprintf(stderr, "%d: registers = ", job->jid);
            for (int i = 0; i < NUMBER_OF_REGISTERS; i++) {
                fprintf(stderr, "%ld ", job->registers[i]);
            }
        }
        fprintf(stderr, "%d: stack = ", job->jid);
        for (int i = 0; i < call_stack_length(&job->call_stack); i++) {
            vm_stack_value_t* value =
                dynarray_element(job->call_stack.stack_array, i);
            fprintf(stderr, "%ld ", *value);
        }
        fprintf(stderr, "==> %d:%d: ", job->jid, job->pc);
        print_instruction(&interpreter->loader->byte_code[job->pc]);
#endif
#endif

        if (instructions_executed++ >= check_after) {
            if (ELAPSED_TIME_MS(start_time) > time_slice) {
                interpreter_result = INTERPRETER_RESULT_TIMEOUT;
                break;
            }
            instructions_executed = 0;
        }
    }

    return interpreter_result;
}

/*
class Interpreter {
    private Loader loader;
    private InterpreterMode interpreterMode;

    this(Loader loader, InterpreterMode interpreterMode) {
        this.loader = loader;
        this.interpreterMode = interpreterMode;
    }

    InterpreterResult run(Scheduler scheduler, Job job,
                          uint64_t time_slice, uint checkAfter) {
        auto startTime = Clock.currTime();
        uint instructionsExecuted = 0;
        InterpreterResult interpreterResult;

        while (true) {
            auto byteCode = loader.byteCode;

            debug(interpreter) {
                if (interpreterMode == InterpreterMode.register) {
                    writefln("%d: registers = %s", job.jid,
                             to!string(job.registers[0 .. 8]));
                }
                writefln("%d: stack = %s", job.jid,
                         to!string(job.callStack.stack));
                writef("==> %d:%d: ", job.jid, job.pc);
                PrettyPrint.printInstruction(&byteCode[job.pc]);
            }

            auto currentPc = job.pc;

            if (++job.pc > byteCode.length) {
                throw new InterpreterError(
                              "Unexpected end of bytecode or invalid jump");
            }

            ubyte* ops = &byteCode[job.pc];

            switch (byteCode[currentPc]) {
            // Register machine instructions
            case Opcode.jmprnze:
                uint size = 0;
                auto register = mixin(Operand!(RegisterType, "ops", "size"));
                auto address = mixin(Operand!(AddressType, "ops", "size"));
                if (job.registers[register] != 0) {
                    job.pc = address;
                } else {
                    job.pc += size;
                }
                break;
            case Opcode.jmpringt:
                uint size = 0;
                auto register = mixin(Operand!(RegisterType, "ops", "size"));
                auto value = mixin(Operand!(ImmediateValueType, "ops", "size"));
                auto address = mixin(Operand!(AddressType, "ops", "size"));
                if (!(job.registers[register] > value)) {
                    job.pc = address;
                } else {
                    job.pc += size;
                }
                break;
            case Opcode.subrri:
                uint size = 0;
                auto firstRegister =
                    mixin(Operand!(RegisterType, "ops", "size"));
                auto secondRegister =
                    mixin(Operand!(RegisterType, "ops", "size"));
                auto value = mixin(Operand!(ImmediateValueType, "ops", "size"));
                job.registers[firstRegister] =
                    job.registers[secondRegister] - value;
                job.pc += size;
                break;
            case Opcode.subrsi:
                uint size = 0;
                auto register = mixin(Operand!(RegisterType, "ops", "size"));
                auto stackOffset =
                    mixin(Operand!(StackOffsetType, "ops", "size"));
                auto value = mixin(Operand!(ImmediateValueType, "ops", "size"));
                job.registers[register] =
                    job.callStack.stack[job.callStack.fp + stackOffset] - value;
                job.pc += size;
                break;
            case Opcode.addrri:
                uint size = 0;
                auto firstRegister =
                    mixin(Operand!(RegisterType, "ops", "size"));
                auto secondRegister =
                    mixin(Operand!(RegisterType, "ops", "size"));
                auto value = mixin(Operand!(ImmediateValueType, "ops", "size"));
                job.registers[firstRegister] =
                    job.registers[secondRegister] + value;
                job.pc += size;
                break;
            case Opcode.loadri:
                uint size = 0;
                auto register = mixin(Operand!(RegisterType, "ops", "size"));
                auto value = mixin(Operand!(ImmediateValueType, "ops", "size"));
                job.registers[register] = value;
                job.pc += size;
                break;
            case Opcode.pushr:
                uint size = 0;
                auto register = mixin(Operand!(RegisterType, "ops", "size"));
                job.callStack.push(job.registers[register]);
                job.pc += size;
                break;
            case Opcode.loadrs:
                uint size = 0;
                auto register = mixin(Operand!(RegisterType, "ops", "size"));
                auto stackOffset =
                    mixin(Operand!(StackOffsetType, "ops", "size"));
                job.registers[register] =
                    job.callStack.stack[job.callStack.fp + stackOffset];
                job.pc += size;
                break;
            case Opcode.loadrr:
                uint size = 0;
                auto firstRegister =
                    mixin(Operand!(RegisterType, "ops", "size"));
                auto secondRegister =
                    mixin(Operand!(RegisterType, "ops", "size"));
                job.registers[firstRegister] = job.registers[secondRegister];
                job.pc += size;
                break;
            case Opcode.rcall:
                uint size = 0;
                // Extract address to function
                auto address = mixin(Operand!(AddressType, "ops", "size"));
                // Push return address onto call stack
                job.callStack.push(job.pc + AddressType.sizeof);
                // Push previous FP onto call stack
                job.callStack.push(job.callStack.fp);
                // Set FP to point at return address
                job.callStack.fp = job.callStack.length - 2;
                // Jump to function address
                job.pc = address;
                break;
            case Opcode.rret:
                // Has call stack been exhausted?
                if (job.callStack.length == 2) {
                    return InterpreterResult.halt;
                }
                auto returnAddress = job.callStack.stack[job.callStack.fp];
                // Remember previous FP
                auto previousFp = job.callStack.stack[job.callStack.fp + 1];
                // Remove call stack frame
                job.callStack.stack =
                    job.callStack.stack[0 .. job.callStack.fp];
                if (job.callStack.length == 1 || returnAddress == -1) {
                    return InterpreterResult.halt;
                }
                // Restore FP to previous FP
                job.callStack.fp = previousFp;
                // Jump to return address
                job.pc = cast(AddressType)returnAddress;
                break;
            case Opcode.jmp:
                uint size = 0;
                auto address = mixin(Operand!(AddressType, "ops", "size"));
                job.pc = address;
                break;
            // Stack machine instructions
            case Opcode.push:
                uint size = 0;
                auto stackValue =
                    mixin(Operand!(StackValueType, "ops", "size"));
                job.callStack.push(stackValue);
                job.pc += size;
                break;
            case Opcode.pushs:
                auto dataPushResult = job.dataStack.push(byteCode[job.pc .. $]);
                auto dataAddress = dataPushResult[0];
                auto length = dataPushResult[1];
                job.callStack.push(dataAddress);
                job.pc += DataLengthType.sizeof + length;
                break;
            case Opcode.pop:
                job.callStack.pop();
                break;
            case Opcode.dup:
                job.callStack.dup();
                break;
            case Opcode.swap:
                job.callStack.swap();
                break;
            case Opcode.load:
                job.callStack.load();
                break;
            case Opcode.store:
                job.callStack.store();
                break;
            case Opcode.add:
                job.callStack.binaryOperation((operand1, operand2) =>
                                              operand1 + operand2);
                break;
            case Opcode.sub:
                job.callStack.binaryOperation((operand1, operand2) =>
                                              operand1 - operand2);
                break;
            case Opcode.mul:
                job.callStack.binaryOperation((operand1, operand2) =>
                                              operand1 * operand2);
                break;
            case Opcode.div:
                job.callStack.binaryOperation((operand1, operand2) =>
                                              operand1 / operand2);
                break;
            case Opcode.jump:
                uint size = 0;
                auto address = mixin(Operand!(AddressType, "ops", "size"));
                job.pc = address;
                break;
            case Opcode.cjump:
                uint size = 0;
                auto conditional = job.callStack.pop();
                if (conditional != 0) {
                    job.pc = mixin(Operand!(AddressType, "ops", "size"));
                } else {
                    job.pc += AddressType.sizeof;
                }
                break;
            case Opcode.call:
                uint size = 0;
                auto address = mixin(Operand!(AddressType, "ops", "size"));
                auto arity = mixin(Operand!(ArityType, "ops", "size"));
                call(job, address, arity, size);
                break;
            case Opcode.mcall:
                auto label = job.callStack.pop();
                auto moduleName = job.callStack.popString();
                auto arity = job.callStack.pop();
                // Ensure that module is loaded
                if (!loader.isModuleLoaded(moduleName)) {
                    loader.loadModule(moduleName);
                    debug(scheduler) {
                        loader.prettyPrint(moduleName);
                    }
                }
                auto address =
                    loader.lookupAddress(moduleName, cast(LabelType)label);
                call(job, cast(AddressType)address, cast(ubyte)arity, 0);
                break;
            case Opcode.ret:
                uint size = 0;
                // Remember essential stack information
                auto returnValue = job.callStack.pop();
                auto arity = job.callStack.stack[job.callStack.fp];
                auto returnAddress = job.callStack.stack[job.callStack.fp + 1];
                auto previousFp = job.callStack.stack[job.callStack.fp + 2];
                // Remove stack frame
                job.callStack.stack =
                    job.callStack.stack[0 .. job.callStack.fp - arity];
                // Push return value onto caller's stack
                auto returnMode =
                    mixin(Operand!(ReturnModeType, "ops", "size"));
                if (returnMode == ReturnMode.copy) {
                    ubyte[] returnData = job.dataStack.peek(returnValue);
                    // Copy data onto caller's data stack
                    auto returnDataCopy = job.dataStack.push(returnData);
                    auto dataAddress = returnDataCopy[0];
                    // Push data address onto caller's stack
                    job.callStack.push(dataAddress);
                } else {
                    // Push return value onto caller's stack
                    job.callStack.push(returnValue);
                }
                // Has stack been exhausted?
                if (job.callStack.length == 1 || returnAddress == -1) {
                    // Just keep result
                    job.callStack.stack = job.callStack.stack[$ - 1 .. $];
                    return InterpreterResult.halt;
                }
                // Remove data stack frame
                auto previousDataFp =
                    Vm.getValue!long(
                        &job.dataStack.stack[job.dataStack.fp]);
                job.dataStack.stack =
                    job.dataStack.stack[0 .. job.dataStack.fp];
                // Restore FP and data FP
                job.callStack.fp = previousFp;
                job.dataStack.fp = previousDataFp;
                // Jump to return address
                job.pc = cast(AddressType)returnAddress;
                break;
            case Opcode.sys:
                uint size = 0;
                auto systemCall =
                    mixin(Operand!(SystemCallType, "ops", "size"));
                final switch (systemCall) {
                case SystemCall.self:
                    job.callStack.push(job.jid);
                    break;
                case SystemCall.send:
                    auto value = job.callStack.pop();
                    auto jid = job.callStack.pop();
                    scheduler.sendMessage(cast(uint)jid, value);
                    job.callStack.push(value);
                    break;
                case SystemCall.recv:
                    if (job.messageBox.length == 0) {
                        --job.pc;
                        return InterpreterResult.recv;
                    }
                    job.callStack.push(job.messageBox.dequeue());
                    break;
                case SystemCall.println:
                    auto s = job.callStack.popString();
                    writeln(s);
                    job.callStack.push(1);
                    break;
                case SystemCall.display:
                    auto topValue = job.callStack.pop();
                    writefln("%d", topValue);
                    job.callStack.push(1);
                    break;
                case SystemCall.exit:
                    return InterpreterResult.exit;
                }
                job.pc += size;
                break;
            case Opcode.and:
                job.callStack.binaryOperation((operand1, operand2) =>
                                              (operand1 != 0) &&
                                              (operand2 == 0) ? 1 : 0);
                break;
            case Opcode.or:
                job.callStack.binaryOperation((operand1, operand2) =>
                                              (operand1 != 0) ||
                                              (operand2 != 0) ? 1 : 0);
                break;
            case Opcode.not:
                auto operand = job.callStack.pop();
                job.callStack.push(!(operand != 0) ? 1 : 0);
                break;
            case Opcode.eq:
                job.callStack.binaryOperation((operand1, operand2) =>
                                              (operand1 == operand2) ? 1 : 0);
                break;
            case Opcode.neq:
                job.callStack.binaryOperation((operand1, operand2) =>
                                              (operand1 != operand2) ? 1 : 0);
                break;
            case Opcode.lt:
                job.callStack.binaryOperation((operand1, operand2) =>
                                              operand1 < operand2 ? 1 : 0);
                break;
            case Opcode.gt:
                job.callStack.binaryOperation((operand1, operand2) =>
                                              operand1 > operand2 ? 1 : 0);
                break;
            case Opcode.nop:
                break;
            case Opcode.halt:
                return InterpreterResult.halt;
            case Opcode.spawn:
                uint size = 0;
                auto address = mixin(Operand!(AddressType, "ops", "size"));
                auto arity = mixin(Operand!(ArityType, "ops", "size"));
                auto parameters =
                    iota(arity).map!(_ => job.callStack.pop()).array.reverse;
                auto jid = spawn(scheduler, address, parameters);
                job.callStack.push(jid);
                job.pc += size;
                break;
            case Opcode.mspawn:
                auto label = job.callStack.pop();
                auto moduleName = job.callStack.popString();
                auto arity = job.callStack.pop();
                auto parameters =
                    iota(arity).map!(_ => job.callStack.pop()).array.reverse;
                auto jid = mspawn(loader, scheduler, moduleName,
                                  cast(LabelType)label, parameters);
                job.callStack.push(jid);
                break;
            default:
                throw new InterpreterError(
                                           "Invalid opcode " ~
                              to!string(byteCode[currentPc]));
            }

            if (instructionsExecuted ++ >= checkAfter) {
                if (Clock.currTime() - startTime >= timeSlice) {
                    interpreterResult = InterpreterResult.timeout;
                    break;
                }
                instructionsExecuted = 0;
            }
        }

        return interpreterResult;
    }

    private uint spawn(Scheduler scheduler, AddressType address,
                       long[] parameters) {
        long arity = parameters.length;
        long returnAddress = -1;
        long fp = -1;
        long[] initialCallStack;

        final switch(interpreterMode) {
        case InterpreterMode.stack:
            initialCallStack = parameters ~ [arity, returnAddress, fp];
            break;
        case InterpreterMode.register:
            initialCallStack = [returnAddress, fp];
        }

        auto jid = Scheduler.nextJid();
        auto job = new Job(jid, address, initialCallStack);

        // Set FP to point to arity
        job.callStack.fp = job.callStack.length - 3;
        scheduler.spawn(job);
        return jid;
    }

    public uint mspawn(Loader loader, Scheduler scheduler, string moduleName,
                       LabelType label, long[] parameters) {
        // Ensure that module is loaded
        if (!loader.isModuleLoaded(moduleName)) {
            loader.loadModule(moduleName);
            debug(scheduler) {
                loader.prettyPrint(moduleName);
            }
        }

        auto address = loader.lookupAddress(moduleName, label);
        return spawn(scheduler, address, parameters);
    }

    void call(Job job, AddressType address, ArityType arity,
              uint sizeOfOperands) {
        // Push arity onto stack
        job.callStack.push(arity);
        // Push return address onto stack
        job.callStack.push(job.pc + sizeOfOperands);
        // Push previous FP onto stack
        job.callStack.push(job.callStack.fp);
        // Set FP to point to arity
        job.callStack.fp = job.callStack.length - 3;
        // Save previous data FP on data stack
        job.dataStack.stack ~=
            (cast(ubyte*)&job.dataStack.fp)[0 .. long.sizeof];
        // Set data FP to previous data FP
        job.dataStack.fp = job.dataStack.length - long.sizeof;
        // Jump to function address
        job.pc = address;
    }
}
*/
