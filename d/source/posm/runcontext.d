module runcontext;

import program;

struct RunContext {
    public ulong id;
    public Program *program;
    public ulong[] stack;
    public ulong fp = 0;
    public ulong pc;

    this(const ulong id, ref Program program, const uint pc) {
        this.id = id;
        this.program = &program;
        this.pc = pc;
    }
}
