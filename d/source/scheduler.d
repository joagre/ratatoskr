module scheduler;

import std.stdio : writeln;
import std.conv : to;
import std.datetime : Duration, msecs;
import std.container : DList;
import core.thread : Thread;
import program;
import interpreter;
import fiber;

struct Scheduler {
    const uint EMPTY_READY_QUEUE_BACK_OFF = 25;

    private Interpreter interpreter;
    private auto readyQueue = DList!Fiber();
    private auto waitingQueue = DList!Fiber();

    private Program[string] programs;

    private Duration timeSlice;
    private uint timeoutGranularity;

    private long fid = 0;

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

        auto fiber = Fiber(fid, program);
        readyQueue.insertBack(fiber);
        return fid++;
    }

    void run() {
        while (!waitingQueue.empty || !readyQueue.empty) {
            while (!readyQueue.empty) {
                auto fiber = readyQueue.front;
                readyQueue.removeFront;
                InterpreterResult result =
                    interpreter.run(this, fiber, timeSlice, timeoutGranularity);
                final switch(result) {
                case InterpreterResult.halt:
                    debug(user) {
                        writeln("Fiber " ~ to!string(fiber.fid) ~ " (" ~
                                fiber.program.filename ~ ") halted: " ~
                                to!string(fiber.stack));
                    }
                    break;
                case InterpreterResult.recv:
                    waitingQueue.insertBack(fiber);
                    break;
                case InterpreterResult.timeout:
                    readyQueue.insertBack(fiber);
                }
            }
            Thread.sleep(msecs(EMPTY_READY_QUEUE_BACK_OFF));
        }
    }
}
