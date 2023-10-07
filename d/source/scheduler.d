module scheduler;

import std.stdio : writeln;
import std.conv : to;
import std.datetime : Duration;
import std.container : DList;
import program;
import interpreter;
import fiber;

struct Scheduler {
    private Interpreter interpreter;
    private auto readyQueue = DList!Fiber();
    private Program[string] programs;
    private Duration timeSlice;
    private uint timeoutGranularity;
    private long fid = 0;

    this(Duration timeSlice, uint timeoutGranularity) {
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
        while (!readyQueue.empty) {
            auto fiber = readyQueue.front;
            readyQueue.removeFront;
            InterpreterResult result =
                interpreter.run(this, fiber, timeSlice, timeoutGranularity);
            final switch(result) {
            case InterpreterResult.halt:
                debug(scheduler) {
                    writeln("Fiber " ~ to!string(fiber.fid) ~ " (" ~
                            fiber.program.filename ~ ") halted: " ~
                            to!string(fiber.stack));
                }
                break;
            case InterpreterResult.timeout:
                readyQueue.insertBack(fiber);
            }
        }
    }
}
