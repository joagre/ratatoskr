import std.stdio : writeln;
import std.string : toStringz;
import _stdc_stdlib = core.stdc.stdlib : malloc, free;
import std.conv : to;
import core.stdc.string : strcpy;

class HeapError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
}

struct Heap {
    private long nextAddress = 0;
    private ubyte*[long] heap;

    long allocate(string content) {
        // +1 for null-terminator
        ubyte* memory = cast(ubyte*)_stdc_stdlib.malloc(content.length + 1);
        if (memory == null) {
            throw new HeapError("Failed to allocate memory.");
        }
        strcpy(cast(char*)memory, toStringz(content));
        heap[nextAddress] = memory;
        return nextAddress++;
    }

    void free(long address) {
        if (address in heap) {
            _stdc_stdlib.free(heap[address]);
            heap.remove(address);
        } else {
            throw new HeapError("Attempted to free an unallocated address.");
        }
    }

    void printHeap() {
        foreach (address, content; heap) {
            writeln(address, " -> ", to!string(content));
        }
    }
}
