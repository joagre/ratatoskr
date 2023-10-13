module scheduler;

import std.stdio : writeln;
import std.conv : to;
import std.datetime : Duration, msecs;
import std.container : DList;
import core.thread : Thread;
import program;
import interpreter;
import job;

struct Scheduler {
    const uint emptyReadyQueueBackOff = 25;

    private Interpreter interpreter;
    private auto readyQueue = DList!Job();
    private auto waitingQueue = DList!Job();

    private Program[string] programs;

    private Duration timeSlice;
    private uint timeoutGranularity;

    private long jid = 0;

    this(ref Interpreter interpreter, Duration timeSlice,
         uint timeoutGranularity) {
        this.interpreter = interpreter;
        this.timeSlice = timeSlice;
        this.timeoutGranularity = timeoutGranularity;
    }

    long spawn(string filename, long[] parameters) {



        Program* program = filename in programs;
        if (program == null) {
            programs[filename] = Program(filename);
            program = &programs[filename];
        }

        debug(scheduler) {
            program.prettyPrint;
        }

        auto job = Job(jid, program);
        job.callStack.append(parameters);
        readyQueue.insertBack(job);
        return jid++;
    }

    void run() {
        while (!waitingQueue.empty || !readyQueue.empty) {
            while (!readyQueue.empty) {
                auto job = readyQueue.front;
                readyQueue.removeFront;
                InterpreterResult result =
                    interpreter.run(this, job, timeSlice, timeoutGranularity);
                final switch(result) {
                case InterpreterResult.halt:
                    debug(user) {
                        writeln("Job " ~ to!string(job.jid) ~ " (" ~
                                job.program.filename ~ ") halted: " ~
                                to!string(job.callStack.stack));
                    }
                    break;
                case InterpreterResult.recv:
                    waitingQueue.insertBack(job);
                    break;
                case InterpreterResult.timeout:
                    readyQueue.insertBack(job);
                }
            }
            Thread.sleep(msecs(emptyReadyQueueBackOff));
        }
    }
}
