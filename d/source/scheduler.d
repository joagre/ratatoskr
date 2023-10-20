module scheduler;

import std.stdio : writeln, writefln;
import std.conv : to;
import std.datetime : Duration, msecs;
import std.container : DList;
import core.thread : Thread;
import std.algorithm : remove;
import std.array;

import interpreter;
import job;
import loader;

class SchedulerError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
}

class Scheduler {
    private uint jid;

    private DList!Job readyQueue;
    private DList!Job waitingQueue;

    private Duration timeSlice;
    private uint checkAfter;

    private Interpreter interpreter;
    private Loader loader;

    private Job runningJob;

    this(Loader loader, Interpreter interpreter, uint timeSlice,
         uint checkAfter) {
        this.jid = 0;
        this.loader = loader;
        this.interpreter = interpreter;
        this.timeSlice = msecs(timeSlice);
        this.checkAfter = checkAfter;
        this.readyQueue = DList!Job();
        this.waitingQueue = DList!Job();
        this.runningJob = null;
    }

    void run() {
        while (!waitingQueue.empty || !readyQueue.empty) {
            while (!readyQueue.empty) {
                auto nextJob = readyQueue.front;
                readyQueue.removeFront;
                nextJob.mode = JobMode.running;
                runningJob = nextJob;
                InterpreterResult result =
                    interpreter.run(this, runningJob, timeSlice, checkAfter);
                final switch(result) {
                case InterpreterResult.halt:
                    debug(user) {
                        writefln("Job %d halted: %s (r0 = %d)", runningJob.jid,
                                 to!string(runningJob.callStack.stack), runningJob.registers[0]);
                    }
                    break;
                case InterpreterResult.recv:
                    runningJob.mode = JobMode.waiting;
                    waitingQueue.insertBack(runningJob);
                    break;
                case InterpreterResult.timeout:
                    runningJob.mode = JobMode.ready;
                    readyQueue.insertBack(runningJob);
                    break;
                case InterpreterResult.exit:
                    return;
                }
            }
            Thread.sleep(timeSlice); // FIXME: A bit ugly
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
        job.mode = JobMode.ready;
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
        auto byteIndex = loader.lookupAddress(moduleName, label);
        auto job = new Job(jid, byteIndex);
        // Push parameters
        job.callStack.append(parameters);
        // Add bogus return address to stack
        job.callStack.push(-1);
        // Save previous fp on the stack
        job.callStack.push(job.callStack.fp);
        // Set fp to first CALL parameter (NOTE: We just pushed two values)
        job.callStack.fp = job.callStack.length - 2 - parameters.length;
        job.mode = JobMode.ready;
        readyQueue.insertBack(job);
        return jid++;
    }

    public void sendMessage(uint jid, long value) {
        auto job = findJob(jid);
        if (job !is null) {
            job.messageBox.enqueue(value);
            if (job.mode == JobMode.waiting) {
                removeFromWaitingQueue(job.jid);
                job.mode = JobMode.ready;
                readyQueue.insertBack(job);
            }
        } else {
            throw new SchedulerError("A job has gone missing!");
        }
    }

    private Job findJob(uint jid) {
        if (runningJob !is null && runningJob.jid == jid) {
            return runningJob;
        }
        foreach(job; readyQueue[]) {
            if(job.jid == jid) {
                return job;
            }
        }
        foreach(job; waitingQueue[]) {
            if(job.jid == jid) {
                return job;
            }
        }
        return null;
    }

    // FIXME: This is very boring!
    private void removeFromWaitingQueue(uint jid) {
        auto arr = waitingQueue[].array;
        arr = arr.remove!(job => job.jid == jid);
        waitingQueue.clear();
        foreach(job; arr) {
            waitingQueue.insertBack(job);
        }
    }
}
