module scheduler.scheduler;

import std.stdio;
import std.conv;
import std.datetime;
import std.container;
import posm.program;
import posm.interpreter;
import scheduler.fiber;

struct Scheduler {
    private Interpreter interpreter;
    private auto readyQueue = DList!Fiber();
    private Program[string] programs;
    private Duration time_slice;
    private uint timeout_granularity;
    private ulong fid = 0;

    this(Duration time_slice, uint timeout_granularity) {
        this.time_slice = time_slice;
        this.timeout_granularity = timeout_granularity;
    }

    ulong spawn(string filename, ulong[] parameters) {
        Program* program = filename in programs;
        if (program == null) {
            programs[filename] = Program(filename);
            program = &programs[filename];
        }

        debug(scheduler) {
            program.pretty_print;
        }

        auto run_context = Fiber(fid, program);
        readyQueue.insertBack(run_context);
        return fid++;
    }

    void run() {
        while (!readyQueue.empty) {
            auto run_context = readyQueue.front;
            readyQueue.removeFront;
            InterpreterResult result =
                interpreter.run(run_context, time_slice, timeout_granularity);
            final switch(result) {
            case InterpreterResult.halt:
                debug(scheduler) {
                    writeln("Fiber " ~ to!string(run_context.fid) ~ " (" ~
                            run_context.program.filename ~ ") halted: " ~
                            to!string(run_context.stack));
                }
                break;
            case InterpreterResult.timeout:
                readyQueue.insertBack(run_context);
            }
        }
    }
}
