# The Ratatoskr Pprogramming Language

The Ratatoskr programming language is based on a Plain Old Stack Machine
(POSM) with special features. Its base instructions does not come
as a surprise: `push, pushs, pop, dup, swap, load, store, add, sub,
mul, div, jump, cjump, call, ret, sys, and, or, not, eq, neq, lt, gt,
nop and halt`. Additional special instructions such as `mcall, spawn,
mspawn, send and recv` (send and receive are actually sys calls for
the moment) and more has been added to cover for the concurrency
oriented and functional flavor of the Ratatoskr programming
language. More on that later.

> [!NOTE]
> This is very much work in progress and starting with the
> stack machine in a bottom up fashion, and at the same time starting
w> ith the target language top down, is a deliberate choice.

# A Plain Old Stack Machine (POSM)

## Instruction set

POSM has two read-only register, `sp` (stack pointer) `fp` (frame
pointer), and for now implements the instructions listed below.

> [!NOTE]
> The stack machine only understands two primitive datatype for now,
> i.e. long and string values. :-)

> [!IMPORTANT]
> The stack grows downwards.

```
label label       ; A jump label. This is a meta instruction.
push value        ; Pushes a value onto the stack.
pushs <string>    ; Pushes a string value onto the stack.
pop               ; Removes the top value from the stack.
dup               ; Duplicates the top value on the stack.
swap              ; Swaps the top two values on the stack.
load sp|fp        ; Copies a value from the stack, using a relative
                  ; sp or fp offset, to the top of the stack. The
                  ; offset is expected to be on the stack.
store sp|fp       ; Updates a value on the stack using a relative sp
                  ; or fp offset. The new value and the offset are
                  ; expected to be on the stack.
                  ; from the stack.
add               ; Pops two values, adds them, and pushes the result.
sub               ; Pops two values, subtracts them, and pushes the result.
mul               ; Pops two values, multiplies them, and pushes the result.
div               ; Pops two values, divides them, and pushes the result.
jump label        ; Unconditionally jumps to a label.
cjump label       ; Conditionally jumps to a label if the top value of
                  ; the stack is not 0.
call label arity  ; Calls a function located in label with the
                  ; specified arity. Pushes the return address and
                  ; previous fp onto the stack. Sets fp to the first
                  ; parameter on the stack. The arity corresponds with
                  ; the number of parameters.
ret [copy]        ; Removes the call stack frame, i.e. everything
                  ; below fp is nuked, and does the same for the data
                  ; stack. Restores fp to the previous fp for both
                  ; stacks. Pushes the return value onto the callers
                  ; call stack. The copy operand is only meaningful if
                  ; the return value is a string, i.e. the string
                  ; return value's data is copied onto the callers
                  ; data stack. Jumps to the return address.
sys name          ; Calls a built-in function, e.g. self, send, recv,
                  ; println, display and exit.
and               ; Pops two values, performs a logical and, and
                  ; pushes the result.
or                ; Pops two values, performs a logical or, and
                  ; pushes the result.
not               ; Pops two values, performs a logical not, and
                  ; pushes the result.
eq                ; Pops two values, checks if they are equal, and
                  ; pushes the result (0 or 1).
neq               ; Pops two values, checks if they are not equal, and
                  ; pushes the result (0 or 1).
lt                ; Pops two values, checks if the first is less than
                  ; the second, and pushes the result (0 or 1).
gt                ; Pops two values, checks if the first is greater than
                  ; the second, and pushes the result (0 or 1).
nop               ; No operation. Do nothing.
halt              ; Halts the stack machine execution.
```

The not so standard instruction are for now:

```
mcall             ; Works as call but label and arity etc are expected
                  ; to be on the stack in the order: number of
                  ; parameters, arity, the string module name and the
                  ; label in that module. The module is loaded
                  ; automatically from disk. After this point the same
                  ; thing happens as described for call.
spawn label arity ; Works as call but spawns a new concurrent job.
mspawn            ; Works as mcall but spawns a new concurrent job.
```

## Examples

Below follows a number of examples and they can be run using the r
command tool:

```
Usage: r [options] <module> <label> [<parameter> ...]
Options:
  -c <instructions>, --check-after=<instructions>
    Check time slice timeout each number of <instructions> (100)
  -h, --help
    Print this message and exit
  -l <directory>, --load-path=<directory>
    Load POSM files from <directory> (./)
  -t <milli-seconds>, --time-slice=<milli-seconds>
    <milli-seconds> spent by each job before context switch (25ms)
```

On to the examples.

### Example: Hello world!

```
__hello_world__start__
; File: ./examples/hello_word.posm
; Run: ./d/bin/r -l ./examples hello_world 0
;
; start() ->
;     io:format("Hello world!\n").

label 0
  pushs "Hello world!"
  sys println
  ret
__hello_world__end__
```

### Example: Factorial

```
__fac__start__
; File: ./examples/fac.posm
; Run: ./d/bin/r -l ./examples fac 0 10
;
; fac(1) ->
;     1;
; fac(N) ->
;     N * fac(N - 1).

label 0          ; fac(1)
  push 0
  load fp        ; N
  push 1
  neq
  cjump 1
  push 1
  ret

label 1          ; fac(N)
  push 0
  load fp        ; N
  push 0
  load fp        ; N
  push 1
  sub            ; N - 1
  call 0 1       ; fac(N - 1).
  mul            ; N * fac(N - 1).
  ret
__fac__end__
```

### Example: Factorial (tail recursive)

```
__tfac__start__
; File: ./examples/tfac.posm
; Run: ./d/bin/r -l ./examples tfac 0 10 1
;
; fac(N) when N >= 0 ->
;     fac(N, 1).
;
; fac(0, Acc) ->
;     Acc;
; fac(N, Acc) when N > 0 ->
;     fac(N - 1, N * Acc).

label 0          ; fac(0, Acc)
  push 0
  load fp        ; N
  push 0
  neq
  cjump 1
  push -1
  load fp        ; Acc
  ret

label 1          ; fac(N, Acc)
  push 0
  load fp        ; N
  push 1
  sub            ; N - 1
  push 0
  load fp        ; N
  push -1
  load fp        ; Ack
  mul
  push -1
  store fp       ; Replace parameter Acc
  push 0
  store fp       ; Replace parameter N
  jump 0         ; fac(N - 1, N * Acc).
__tfac__end__
```

### Dynamic code loading

```
__module_calls__start__
; File: ./examples/module_calls.posm
; Run: ./d/bin/r -l ./examples module_calls 0 10
;
; start(N) ->
;     io:format("~w\n", [fac:fac(N)]),
;     io:format("~w\n", [tfac:fac(N + 2)]).

label 0          ; start(N)
  push 0
  load fp        ; N
  push 1         ; Arity
  pushs "fac"    ; Module name
  push 0
  mcall          ; fac:fac(N)
  sys display
  pop
  push 0
  load fp        ; N
  push 2
  add            ; N + 2
  push 1         ; Acc
  push 2         ; Arity
  pushs "tfac"   ; Module name
  push 0         ; Label
  mcall          ; tfac:fac(N, 1)
  sys display
  ret
__module_calls__end__
```

### Example: Concurrent Ackermann

```
__ackermann__start__
; File: ./examples/ackermann.posm
; Run: ./d/bin/r -l ./examples ackermann 0
; Run: ./d/bin/r -l ./examples ackermann 10
; Run: ./d/bin/r -l ./examples ackermann 1 3 6
;
; start() ->
;     ackermann(3, 6).
;
; start2() ->
;     spawn(fun() -> ackermann(3, 6) end),
;     spawn(fun() -> ackermann(3, 7) end),
;     spawn(fun() -> fac:fac(10) end).
;
; ackermann(0, N) ->
;     N + 1;
; ackermann(M, 0) when M > 0 ->
;     ackermann(M - 1, 1);
; ackermann(M, N) when M > 0, N > 0 ->
;     ackermann(M - 1, ackermann(M, N - 1)).

label 0          ; start()
  push 3         ; M
  push 6         ; N
  call 1 2       ; ackermann(3, 6)
  dup
  sys display
  pop
  ret

label 10         ; start2()
  push 3         ; M
  push 6         ; N
  spawn 1 2      ; spawn(fun() -> ackermann(3, 6) end)
  pop
  push 3         ; M
  push 7         ; N
  spawn 1 2      ; spawn(fun() -> ackermann(3, 7) end)
  pop
  push 10        ; N
  push 1         ; Arity
  pushs "fac"    ; Module name
  push 0         ; Label
  mspawn         ; spawn(fun() -> fac:fac(10) end)
  ret

label 1          ; ackermann(0, N)
  push 0
  load fp        ; M
  push 0
  neq            ; M == 0?
  cjump 2
  push -1
  load fp        ; N
  push 1
  add            ; N + 1
  ret

label 2          ; ackermann(M, 0) when M > 0
  push -1
  load fp        ; N
  push 0
  neq
  cjump 3
  push 0
  load fp        ; M
  push 0
  gt             ; M > 0 ?
  not
  cjump 3
  push 0         ; M
  load fp
  push 1
  sub            ; M - 1
  push 0
  store fp       ; Replace parameter M
  push 1
  push -1
  store fp       ; Replace parameter N
  jump 1         ; ackermann(M - 1, 1);

label 3          ; ackermann(M, N) when M > 0, N > 0
  push 0
  load fp        ; M
  push 1
  sub            ; M - 1
  push 0
  load fp        ; M
  push -1
  load fp        ; N
  push 1
  sub            ; N - 1
  call 1 2       ; ackermann(M, N - 1)
  push -1
  store fp       ; Replace parameter N
  push 0
  store fp       ; Replace parameter M
  jump 1         ; ackermann(M - 1, ackermann(M, N - 1))
__ackermann__end__
```

### Message passing

```
__message_passing__start__
; File: ./examples/message_passing.posm
; Run: ./d/bin/r -l ./examples message_passing 0 7
;
; start(N) ->
;     spawn_all(self(), N),
;     wait_for_all(N).
;
; spawn_all(_Self, 0) ->
;     io:format("All jobs have been started\n");
; spawn_all(Self, N) ->
;     spawn(fun() -> Self ! ackermann:ackermann(3, N) end),
;     spawn_all(Self, N - 1).
;
; wait_for_all(0) ->
;     io:format("All jobs have returned a result\n");
; wait_for_all(N) ->
;     receive
;         Result ->
;             io:format("~w\n",  [Result]),
;             wait_for_all(N - 1)
;    end.

label 0          ; start(N)
  sys self       ; self()
  push 0
  load fp        ; N
  call 10 2      ; spawn_all(self(), N),
  push 0
  load fp        ; N
  call 20 1      ; wait_for_all(N).
  ret

label 10         ; spawn_all(_Self, 0)
  push -1
  loadf fp       ; N
  push 0
  neq
  cjump 11
  pushs "All jobs have been started"
  sys println
  ret

label 11         ; spawn_all(Self, N)
  push 0
  load fp        ; Self
  load -1
  load fp        ; N
  spawn 12 2     ; fun() -> Self ! ackermann:ackermann(3, N) end)
  push 0
  load fp        ; Self
  push -1
  load fp        ; N
  push 1
  sub            ; N - 1
  call 10 2       ; spawn_all(Self, N - 1)
  ret

label 12         ; ackermann:ackermann(3, N)
  push 3         ; M
  push -1
  load fp        ; N
  push 2         ; Arity
  pushs "ackermann"
  push 1         ; Label
  mcall          ; ackermann:ackermann(3, N)
  push 0
  load fp        ; Self
  swap
  sys send
  ret

label 20         ; wait_for_all(0)
  push 0
  loadf fp       ; N
  push 0
  neq
  cjump 21
  pushs "All jobs have returned a result"
  sys println
  ret

label 21         ; wait_for_all(N)
  sys recv
  sys display
  push 0
  load fp        ; N
  push 1
  sub            ; N - 1
  call 20        ; wait_for_all(N - 1)
  ret
__message_passing__start__
```
