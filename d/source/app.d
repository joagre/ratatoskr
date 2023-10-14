import std.stdio;
import std.path;
import std.conv;
import std.getopt;
import std.algorithm;
import std.array;

import interpreter;
import loader;
import scheduler;

int main(string[] args) {
    uint timeSlice = 25;
    ushort checkAfter = 100;
    string loadPath = "./";

    string usage =
        "Usage: " ~ baseName(args[0]) ~ " [options] <module> <label> [parameter ...]\n" ~
        "Options:\n" ~
        "  -c <instructions>, --check-after=<instructions>\n" ~
        "    Check time slice after a number of <instructions> (" ~ to!string(checkAfter) ~ ")\n" ~
        "  -h, --help\n" ~
        "    Print this message and exit\n" ~
        "  -l <directory>, --load-path=<directory>\n" ~
        "    Load POSM files from <directory> (" ~ loadPath ~ ")\n" ~
        "  -t <milli-seconds>, --time-slice=<milli-seconds>\n" ~
        "    <milli-seconds> spent by each job before context switch (" ~ to!string(timeSlice) ~ "ms)";

    enum ReturnCode : int {
        SUCCESS = 0,
        PARAMETER_ERROR = 1,
        LOADER_ERROR = 2,
        INTERPRETER_ERROR = 3,
        UNEXPECTED_ERROR = 4
    }

    try {
        auto helpInformation =
            args.getopt("t|time-slice", &timeSlice,
                        "c|check-after", &checkAfter,
                        "l|load-path", &loadPath);
        if (helpInformation.helpWanted) {
            stderr.writeln(usage);
            return ReturnCode.PARAMETER_ERROR;
        }
    } catch (ConvException e) {
        stderr.writeln("Parameter error: " ~ e.msg);
        return ReturnCode.PARAMETER_ERROR;
    } catch (GetOptException e) {
        stderr.writeln("Parameter error: " ~ e.msg);
        stderr.writeln(usage);
        return ReturnCode.PARAMETER_ERROR;
    }

    string moduleName;
    uint label;
    long[] parameters;

    if (args.length < 3) {
        stderr.writeln(usage);
        return ReturnCode.PARAMETER_ERROR;
    }

    moduleName = args[1];
    try {
        label = to!uint(args[2]);
        parameters = args[3 .. $].map!(s => s.to!long).array;
    } catch (ConvException e) {
        stderr.writeln("Parameter error: " ~ e.msg);
        stderr.writeln(usage);
        return ReturnCode.PARAMETER_ERROR;
    }

    try {
        auto loader = new Loader(loadPath);
        auto interpreter = new Interpreter(loader);
        auto scheduler = new Scheduler(loader, interpreter, timeSlice, checkAfter);
        scheduler.mspawn(moduleName, label, parameters);
        scheduler.run();
    } catch (LoaderError e) {
        stderr.writeln("Loader error: ", e.msg);
        return ReturnCode.LOADER_ERROR;
    } catch (InterpreterError e) {
        stderr.writeln("Interpreter error: ", e.msg);
        return ReturnCode.INTERPRETER_ERROR;
    } catch (Exception e) {
        stderr.writeln("Unexpected error: ", e.msg);
        return ReturnCode.UNEXPECTED_ERROR;
    }

    return 0;
}
