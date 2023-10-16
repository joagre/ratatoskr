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

    public void sendMessage(uint jid, long value) {
        auto job = findJob(jid);
        if (job !is null) {
            job.messageBox.enqueue(value);
            if (job.mode == JobMode.waiting) {
                removeFromWaitingQueue(job);
                readyQueue.insertBack(job);
            }
        }
    }

    private void removeFromWaitingQueue(Job job) {
        auto range = waitingQueue[];
        while (!range.empty) {
            if (range.front is job) {
                waitingQueue.remove(range);
                break;
            }
            range.popFront();
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

    void run() {
        while (!waitingQueue.empty || !readyQueue.empty) {
            while (!readyQueue.empty) {
                auto nextJob = readyQueue.front;
                readyQueue.removeFront;
                nextJob.mode = JobMode.running;
                runningJob = nextJob;
                InterpreterResult result =
                    interpreter.run(this, nextJob, timeSlice, checkAfter);
                final switch(result) {
                case InterpreterResult.halt:
                    debug(user) {
                        writefln("Job %d halted: %s",  runningJob.jid,
                                 to!string(runningJob.callStack.stack));
                    }
                    break;
                case InterpreterResult.recv:
                    waitingQueue.insertBack(runningJob);
                    runningJob.mode = JobMode.waiting;
                    break;
                case InterpreterResult.timeout:
                    readyQueue.insertBack(runningJob);
                    runningJob.mode = JobMode.ready;
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
