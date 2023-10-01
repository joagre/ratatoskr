module runcontext;

struct RunContext {
    public ulong[] stack;
    public ulong fp = 0;
    public ulong pc;

    this(const uint pc) {
        this.pc = pc;
    }
}
