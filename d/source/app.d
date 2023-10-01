import std.stdio;
import std.datetime;
import std.path;
import std.conv;
import std.container;
import program;
import runcontext;
import interpreter;

int main(const string[] args) {
    if (args.length < 6) {
        stderr.writeln("Usage " ~ baseName(args[0]) ~
                       ": <filename> <pc> <duration> <instructions-per-check> <number-of-contexts>");
        return 1;
    }

    try {
        auto pc = to!uint(args[2]);
        auto duration = msecs(to!long(args[3]));
        auto instructions_per_check = to!uint(args[4]);
        auto number_of_contexts = to!uint(args[5]);

        // Load program from file
        Program program = Program(args[1]);
        //program.pretty_print;

        // Populate the ready queue with run contexts
        auto readyQueue = DList!RunContext();
        for (int i = 0; i < number_of_contexts; i++) {
            auto run_context = RunContext(i, program, pc);
            readyQueue.insertBack(run_context);
        }

        // Run the ready queue until empty
        Interpreter interpreter;
        while (!readyQueue.empty) {
            auto run_context = readyQueue.front;
            readyQueue.removeFront;
            RunResult result =
                interpreter.run(run_context, duration, instructions_per_check);
            if (result == RunResult.halt) {
                writeln(to!string(run_context.stack) ~ "(fiber " ~
                        to!string(run_context.id) ~ ")");
            } else { // RunResult.timeout
                readyQueue.insertBack(run_context);
            }
        }

        return 0;
    } catch (Exception e) {
        writeln(e.msg);
        return 2;
    }
}
