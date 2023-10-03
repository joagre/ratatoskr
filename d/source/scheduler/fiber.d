module scheduler.fiber;

import posm.program;

struct Fiber {
    public ulong fid;
    public Program* program;
    public ulong[] stack;
    // sp is always stack.length - 1
    public ulong fp = 0;
    public ulong pc = 0;

    this(ulong fid, Program* program) {
        this.fid = fid;
        this.program = program;
    }
}
