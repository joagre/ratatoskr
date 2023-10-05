module fiber;

import program;

struct Fiber {
    public ulong fid;
    public Program* program;
    public long[] stack;
    // sp is always stack.length - 1
    public long fp = 0;
    public long pc = 0;

    this(ulong fid, Program* program) {
        this.fid = fid;
        this.program = program;
    }
}
