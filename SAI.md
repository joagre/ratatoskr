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
  pushstr "Hello World!"
  loadrs r1 @0
  sys println
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

label 10
  call 0
  loadrr r1 r0
  sys display
  ret

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
; Run: sa tfac 0 10 1
;
; fac(N) when N >= 0 ->
;     fac(N, 1).
;
; fac(0, Acc) ->
;     Acc;
; fac(N, Acc) when N > 0 ->
;     fac(N - 1, N * Acc).

label 10
  call 0
  loadrr r1 r0
  sys display
  ret

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

## Example: Ackermann

```
; Run: sa ackermann 0 3 6
;
; ackermann(0, N) ->
;     N + 1;
; ackermann(M, 0) ->
;     ackermann(M - 1, 1);
; ackermann(M, N) ->
;     ackermann(M - 1, ackermann(M, N - 1)).

label 10
  call 0
  loadrr r1 r0
  sys display
  ret

label 0            ; ackermann(0, N)
  jmprnze r1 1
  addrri r0 r2 #1
  ret

label 1            ; ackermann(M, 0)
  jmprnze r2 2
  subrri r1 r1 #1
  loadri r2 #1
  jmp 0            ; ackermann(M - 1, 1);

label 2            ; Clause ackermann(M, N)
  pushr r1
  subrri r2 r2 #1
  call 0          ; ackermann(M, N - 1)
  subrsi r1 @0 #1
  loadrr r2 r0
  pop
  jmp 0            ; ackermann(M - 1, ackermann(M, N - 1)).
```

## Example: Inter-module calling

```
; Run: sa -l ./ mcall 0 10
;
; start(N) ->
;     io:format("~w\n", [fac:fac(N)]),
;     io:format("~w\n", [tfac:fac(N + 2)]).

label 0
   loadrr r3 r1    ; Remember N
   pushi #0        ; Push label
   pushstr "fac"   ; Push module name
   mcall
   loadrr r1 r0
   sys display     ; Print result
   addrri r1 r3 #2 ;
   pushi #0        ; Push label
   pushstr "tfac"  ; Push module name
   mcall
   loadrr r1 r0
   sys display     ; Print result
   ret
```

## Example: Inter-module spawning

```
; Run: sa mspawn 0 3 6
;
; start(M, N) ->
;     spawn(fun() -> ackermannr:calc(M, N) end),
;     spawn(fun() -> ackermannr:calc(M, N) end),
;     spawn(fun() -> fac:calc(10) end).

label 0
  pushi #2
  pushi #10
  pushstr "ackermannr"
  mspawn         ; spawn(fun() -> ackermann:calc(M, N) end)
  pushi #2
  pushi #10
  pushstr "ackermannr"
  mspawn         ; spawn(fun() -> ackermann:calc(M, N) end)
  pushi #1
  pushi #10
  pushstr "fac"
  loadri r1 #10
  mspawn         ; spawn(fun() -> fac:calc(10) end)
  ret
```

## Example: Message passing

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

label 0           ; start(N)
  pushr r1        ; Remember N
  sys self        ; self()
  popr r1
  loadrs r2 @0    ; N
  call 10         ; spawn_all(self(), N),
  popr r1         ; N
  call 20         ; wait_for_all(N).
  ret

label 10          ; spawn_all(_Self, 0)
  jmprnze r2 11
  pushstr "All jobs have been started"
  popr r1
  sys println
  ret

label 11          ; spawn_all(Self, N)
  pushr r2        ; Remember N
  pushr r1        ; Remember Self
  pushi #2
  spawn 12        ; fun() -> Self ! ackermann:ackermann(3, N) end)
  popr r1         ; Self
  popr r2         ; N
  subrri r2 r2 #1 ; N - 1
  call 10         ; spawn_all(Self, N - 1)
  ret

label 12          ; fun(Self, N)
  pushr r1        ; Remember Self
  pushr r2        ; Remember N
  loadri r1 #3
  popr r2
  pushi #0
  pushstr "ackermannr"
  mcall
  popr r1
  loadrr r2 r0
  sys send
  ret

label 20          ; wait_for_all(0)
  jmprnze r1 21
  pushstr "All jobs have returned a result"
  popr r1
  sys println
  ret

label 21          ; wait_for_all(N)
  pushr r1        ; Remember N
  sys recv
  loadrr r1 r0
  sys display
  popr r1
  subrri r1 r1 #1
  call 20         ; wait_for_all(N - 1)
  ret
```
