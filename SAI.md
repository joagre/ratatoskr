# Satie's byte code interpreter

The Satie compiler produces byte code suitable to be executed by the
interpeter described in this document. This is currently and ongoing
experiment and the first prototype reincarnation was built as a plain
stack machine but work is underway to switch to a register machine.

# Instruction set

> [!IMPORTANT]
> The stack grows downwards.

```
label label       ; A jump label. This is a meta instruction.
push value        ; Pushes a value onto the stack.
pushs <string>    ; Pushes a string value onto the stack.
pop               ; Removes a value from the stack.
dup               ; Duplicates the value on the stack.
swap              ; Swaps two values on the stack.
load              ; Copies a value onto the from an offset relative
                  ; the frame pointer given as a value on the stack.
store             ; Updates a value on the stack in a position given
                  ; by as an offset relative to the frame pointer, and
                  ; with the value on top of tyhe stack.
                  ; new value is alsand the offset are
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

The not so standard instructions are for now:

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

# Examples

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

## Example: Hello world!

```
; Run: sa hello_world 0

label 0
  pushi #42
  sys display
  ret
```

## Example: Factorial

```
; Run: sa fac 0 10
;
; fac(1) ->
;     1;
; fac(N) ->
;     N * fac(N - 1).

label 0            ; fac(1)
  jmprineq r1 #1 1
  loadri r0 #1
  ret

label 1            ; fac(N)
  pushr r1
  subrri r1 r1 #1
  call 0
  popr r1
  mulrrr r0 r1 r0
  ret
```

## Example: Factorial (tail recursive)

```
; Run: sa -l ./ tfac 0 10 1
;
; fac(N) when N >= 0 ->
;     fac(N, 1).
;
; fac(0, Acc) ->
;     Acc;
; fac(N, Acc) when N > 0 ->
;     fac(N - 1, N * Acc).

label 0          ; fac(N)
  loadri r2 #1

label 1          ; fac(0, Acc)
  jmprnze r1 2
  loadrr r0 r2
  ret

label 2            ; fac(N, Acc)
  loadrr r3 r1     ; Save N
  subrri r1 r1 #1  ; Replace parameter N
  mulrrr r2 r3 r2  ; Replace parameter Acc
  jmp 1            ; fac(N - 1, N * Acc)
```

## Dynamic code loading

```
; Run: sa -l ./ module_calls 0 10
;
; start(N) ->
;     io:format("~w\n", [fac:fac(N)]),
;     io:format("~w\n", [tfac:fac(N + 2)]).

label 0          ; start(N)
  push -1
  load           ; N
  push 1         ; Arity
  pushs "fac"    ; Module name
  push 0
  mcall          ; fac:fac(N)
  sys display
  pop
  push -1
  load           ; N
  push 2
  add            ; N + 2
  push 1         ; Acc
  push 2         ; Arity
  pushs "tfac"   ; Module name
  push 0         ; Label
  mcall          ; tfac:fac(N, 1)
  sys display
  ret
```

## Example: Concurrent Ackermann

```
; Run: sa -l ./ ackermann 0
; Run: sa -l ./ ackermann 10
; Run: sa -l ./ ackermann 1 3 6
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
  push -2
  load           ; M
  push 0
  neq            ; M == 0?
  cjump 2
  push -1
  load           ; N
  push 1
  add            ; N + 1
  ret

label 2          ; ackermann(M, 0) when M > 0
  push -1
  load           ; N
  push 0
  neq
  cjump 3
  push -2
  load           ; M
  push 0
  gt             ; M > 0 ?
  not
  cjump 3
  push -2        ; M
  load
  push 1
  sub            ; M - 1
  push -2
  store          ; Replace parameter M
  push 1
  push -1
  store          ; Replace parameter N
  jump 1         ; ackermann(M - 1, 1);

label 3          ; ackermann(M, N) when M > 0, N > 0
  push -2
  load           ; M
  push 1
  sub            ; M - 1
  push -2
  load           ; M
  push -1
  load           ; N
  push 1
  sub            ; N - 1
  call 1 2       ; ackermann(M, N - 1)
  push -1
  store          ; Replace parameter N
  push -2
  store          ; Replace parameter M
  jump 1         ; ackermann(M - 1, ackermann(M, N - 1))
```

## Message passing

```
; Run: sa -l ./ message_passing 0 7
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
  push -1
  load           ; N
  call 10 2      ; spawn_all(self(), N),
  pop
  push -1
  load           ; N
  call 20 1      ; wait_for_all(N).
  ret

label 10         ; spawn_all(_Self, 0)
  push -1
  load           ; N
  push 0
  neq
  cjump 11
  pushs "All jobs have been started"
  sys println
  ret

label 11         ; spawn_all(Self, N)
  push -2
  load           ; Self
  push -1
  load           ; N
  spawn 12 2     ; fun() -> Self ! ackermann:ackermann(3, N) end)
  pop
  push -2
  load           ; Self
  push -1
  load           ; N
  push 1
  sub            ; N - 1
  call 10 2      ; spawn_all(Self, N - 1)
  ret

label 12         ; fun(Self, N)
  push 3         ; M
  push -1
  load           ; N
  push 2         ; Arity
  pushs "ackermann"
  push 1         ; Label
  mcall          ; ackermann:ackermann(3, N)
  push -2
  load           ; Self
  swap
  sys send
  ret

label 20         ; wait_for_all(0)
  push -1
  load           ; N
  push 0
  neq
  cjump 21
  pushs "All jobs have returned a result"
  sys println
  ret

label 21         ; wait_for_all(N)
  sys recv
  sys display
  pop
  push -1
  load           ; N
  push 1
  sub            ; N - 1
  call 20 1      ; wait_for_all(N - 1)
  ret
```
