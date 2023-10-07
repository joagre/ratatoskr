module fiber;

import program;

struct Fiber {
    public long fid;
    public Program* program;
    // Program Counter
    public long PC = 0;

    // Call stack
    public long[] stack;
    // Frame pointer
    public long FP = 0;

    // Data stack
    public ubyte[] dataStack;
    // Data Frame Pointer
    public long DFP = 0;

    this(long fid, Program* program) {
        this.fid = fid;
        this.program = program;
    }
}
