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

    uint spawn(uint byteIndex, long[] parameters) {
        auto job = new Job(jid, byteIndex);
        long[] bogusStackFrame = [-1, -1];
        job.callStack.set(parameters ~ bogusStackFrame);
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
        long[] bogusStackFrame = [-1, -1];
        job.callStack.set(parameters ~ bogusStackFrame);
        readyQueue.insertBack(job);
        return jid++;
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
}
