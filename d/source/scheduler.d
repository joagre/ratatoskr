module scheduler;

import std.stdio : writeln, writefln;
import std.conv : to;
import std.datetime : Duration, msecs;
import std.container : DList;
import core.thread : Thread;

import interpreter;
import job;
import loader;

class Scheduler {
    private uint jid = 0;

    private auto readyQueue = DList!Job();
    private auto waitingQueue = DList!Job();

    private Duration timeSlice;
    private uint checkAfter;

    private Interpreter interpreter;
    private Loader loader;

    this(Loader loader, Interpreter interpreter, uint timeSlice,
         uint checkAfter) {
        this.loader = loader;
        this.interpreter = interpreter;
        this.timeSlice = msecs(timeSlice);
        this.checkAfter = checkAfter;
    }

    void run() {
        while (!waitingQueue.empty || !readyQueue.empty) {
            while (!readyQueue.empty) {
                auto job = readyQueue.front;
                readyQueue.removeFront;
                InterpreterResult result =
                    interpreter.run(this, job, timeSlice, checkAfter);
                final switch(result) {
                case InterpreterResult.halt:
                    debug(user) {
                        writefln("Job %d halted: %s",  job.jid,
                                 to!string(job.callStack.stack));
                    }
                    break;
                case InterpreterResult.recv:
                    waitingQueue.insertBack(job);
                    break;
                case InterpreterResult.timeout:
                    readyQueue.insertBack(job);
                    break;
                case InterpreterResult.exit:
                    return;
                }
            }
            Thread.sleep(timeSlice);
        }
    }

    uint spawn(uint byteIndex, long[] parameters) {
        auto job = new Job(jid, byteIndex);
        // Push parameters
        job.callStack.append(parameters);
        // Add bogus return address to stack
        job.callStack.push(-1);
        // Save previous fp on the stack
        job.callStack.push(job.callStack.fp);
        // Set fp to first CALL parameter (NOTE: We just pushed two values)
        job.callStack.fp = job.callStack.length - 2 - parameters.length;
        readyQueue.insertBack(job);
        return jid++;
    }

    uint mspawn(string moduleName, uint label, long[] parameters) {
        if (!loader.isModuleLoaded(moduleName)) {
            loader.loadPOSMCode(moduleName);
            debug(scheduler) {
                loader.prettyPrint(moduleName);
            }
        }
        auto byteIndex = loader.lookupByteIndex(moduleName, label);
        auto job = new Job(jid, byteIndex);
        // Push parameters
        job.callStack.append(parameters);
        // Add bogus return address to stack
        job.callStack.push(-1);
        // Save previous fp on the stack
        job.callStack.push(job.callStack.fp);
        // Set fp to first CALL parameter (NOTE: We just pushed two values)
        job.callStack.fp = job.callStack.length - 2 - parameters.length;
        readyQueue.insertBack(job);
        return jid++;
    }
}
