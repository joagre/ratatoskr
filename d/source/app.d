import std.stdio;
import std.datetime;
import std.path;
import std.conv;
import std.traits;
import program;
import runcontext;
import interpreter;

int main(const string[] args) {
    if (args.length < 5) {
        stderr.writeln("Usage " ~ baseName(args[0]) ~
                       ": <filename> <pc> <duration> <instructions-per-check>");
        return 1;
    }

    try {
        Program program = Program(args[1]);
        auto pc = to!uint(args[2]);
        auto duration = msecs(to!long(args[3]));
        auto instructions_per_check = to!uint(args[4]);
        //program.pretty_print();
        RunContext run_context = RunContext(pc);
        Interpreter interpreter = Interpreter(program, run_context);
        RunResult result = interpreter.run(duration, instructions_per_check);
        writeln("Result: " ~ enumToName(result));
        writeln("Stack: " ~ to!string(run_context.stack));
        return 0;
    } catch (Exception e) {
        writeln(e.msg);
        return 2;
    }
}

string enumToName(T)(T value) {
    return to!string([EnumMembers!T][value]);
}
