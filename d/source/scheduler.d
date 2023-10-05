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
    private Duration time_slice;
    private uint timeout_granularity;
    private long fid = 0;

    this(Duration time_slice, uint timeout_granularity) {
        this.time_slice = time_slice;
        this.timeout_granularity = timeout_granularity;
    }

    long spawn(string filename, long[] parameters) {
        Program* program = filename in programs;
        if (program == null) {
            programs[filename] = Program(filename);
            program = &programs[filename];
        }

        debug(scheduler) {
            program.pretty_print;
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
                interpreter.run(this, fiber, time_slice, timeout_granularity);
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
