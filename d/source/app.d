import std.stdio;
import std.file;
import std.path;
import std.datetime;
import std.conv;
import scheduler;
import program : ByteCodeError;
import interpreter : Interpreter, InterpreterError;
import core.stdc.stdlib: exit;
import loader;

int main(const string[] args) {
    /*
    Loader loader = Loader("./doc");
    loader.loadPOSMCode("ackermann");
    loader.loadPOSMCode("example1");
    loader.prettyPrint("example1");
    exit(2);
    */














    if (args.length != 4) {
        stderr.writeln("Usage " ~ baseName(args[0]) ~
                       ": <filename> <time-slice> <timeout-granularity>");
        return 1;
    }

    auto filename = args[1];
    if (!exists(filename)) {
        stderr.writeln("Parameter error: " ~ filename ~ " does not exist");
        return 2;
    }

    Interpreter interpreter;

    try {
        auto timeSlice = msecs(to!uint(args[2]));
        auto timeoutGranularity = to!uint(args[3]);
        auto scheduler = Scheduler(interpreter, timeSlice, timeoutGranularity);
        scheduler.spawn(filename, []);
        scheduler.run();
    } catch (ConvException e) {
        stderr.writeln("Parameter error: ", e.msg);
        return 3;
    } catch (ByteCodeError e) {
        stderr.writeln("Byte code error: ", e.msg);
        return 4;
    } catch (InterpreterError e) {
        stderr.writeln("Interpreter error: ", e.msg);
        return 5;
    } catch (Exception e) {
        stderr.writeln("Unexpected error: ", e.msg);
        return 6;
    }
    return 0;
}
