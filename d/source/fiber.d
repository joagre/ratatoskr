module fiber;

import program;

struct Fiber {
    public long fid;
    public Program* program;
    // Program Counter
    public long pc = 0;
    // Call stack
    public long[] stack;
    // Frame pointer
    public long fp = 0;
    // Data stack
    public ubyte[] data_stack;
    // Data Frame Pointer
    public long data_fp = 0;

    this(long fid, Program* program) {
        this.fid = fid;
        this.program = program;
    }
}
