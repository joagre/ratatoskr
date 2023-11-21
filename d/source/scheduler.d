module scheduler;

import std.stdio;
import std.container;
import std.datetime;
import std.conv;
import core.thread;
import std.algorithm;
import std.range;

import interpreter;
import job;
import loader;

class SchedulerError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
}

class Scheduler {
    static uint jid = 0;

    private DList!Job readyQueue;
    private DList!Job waitingQueue;

    private Duration timeSlice;
    private uint checkAfter;

    private Interpreter interpreter;
    private Loader loader;

    private Job runningJob;

    this(Loader loader, Interpreter interpreter, uint timeSlice,
         uint checkAfter) {
        this.loader = loader;
        this.interpreter = interpreter;
        this.timeSlice = msecs(timeSlice);
        this.checkAfter = checkAfter;
        this.readyQueue = DList!Job();
        this.waitingQueue = DList!Job();
        this.runningJob = null;
    }

    static uint nextJid() {
        return jid++;
    }


    /*
    public void foo() {
        while (true) {
        }
    }

    public void foo() {
        while (true) {
        };
    }
    */



    public void run() {
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
                        writefln(
                            "Job %d halted: %s (r0 = %d)",
                            runningJob.jid,
                            to!string(runningJob.callStack.stack),
                            runningJob.registers[0]);
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

    public void spawn(Job job) {
        job.mode = JobMode.ready;
        readyQueue.insertBack(job);
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

    private void removeFromWaitingQueue(uint jid) {
        auto allJobs = waitingQueue[];
        auto foundJobs = find!(job => job.jid == jid)(allJobs);
        auto toRemove = take(foundJobs, 1);
        if (!toRemove.empty) {
            waitingQueue.linearRemove(toRemove);
        }
    }
}
