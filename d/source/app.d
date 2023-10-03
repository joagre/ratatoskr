import std.stdio;
import std.file;
import std.path;
import std.datetime;
import std.conv;
import scheduler;
import program : ByteCodeError;
import interpreter : InterpreterError;

int main(const string[] args) {
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

    try {
        auto time_slice = msecs(to!uint(args[2]));
        auto timeout_granularity = to!uint(args[3]);
        auto scheduler = Scheduler(time_slice, timeout_granularity);
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
