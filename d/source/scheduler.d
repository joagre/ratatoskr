module scheduler;

import std.stdio : writeln;
import std.conv : to;
import std.datetime : Duration, msecs;
import std.container : DList;
import core.thread : Thread;
import program;
import interpreter;
import runcontext;

struct Scheduler {
    const uint emptyReadyQueueBackOff = 25;

    private Interpreter interpreter;
    private auto readyQueue = DList!RunContext();
    private auto waitingQueue = DList!RunContext();

    private Program[string] programs;

    private Duration timeSlice;
    private uint timeoutGranularity;

    private long rcid = 0;

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

        auto runContext = RunContext(rcid, program);
        runContext.callStack.append(parameters);
        readyQueue.insertBack(runContext);
        return rcid++;
    }

    void run() {
        while (!waitingQueue.empty || !readyQueue.empty) {
            while (!readyQueue.empty) {
                auto runContext = readyQueue.front;
                readyQueue.removeFront;
                InterpreterResult result =
                    interpreter.run(this, runContext, timeSlice,
                                    timeoutGranularity);
                final switch(result) {
                case InterpreterResult.halt:
                    debug(user) {
                        writeln("RunContext " ~ to!string(runContext.rcid) ~
                                " (" ~ runContext.program.filename ~
                                ") halted: " ~ to!string(runContext.callStack.stack));
                    }
                    break;
                case InterpreterResult.recv:
                    waitingQueue.insertBack(runContext);
                    break;
                case InterpreterResult.timeout:
                    readyQueue.insertBack(runContext);
                }
            }
            Thread.sleep(msecs(emptyReadyQueueBackOff));
        }
    }
}
