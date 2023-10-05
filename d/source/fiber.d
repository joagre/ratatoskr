module fiber;

import program;

struct Fiber {
    public long fid;
    public Program* program;
    public long[] stack;
    public ubyte[] dstack;
    // sp is always stack.length - 1
    public long fp = 0;
    public long dfp = 0;
    public long pc = 0;

    this(long fid, Program* program) {
        this.fid = fid;
        this.program = program;
    }
}
