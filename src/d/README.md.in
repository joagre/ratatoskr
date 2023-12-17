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
__hello_world.posm__
```

## Example: Factorial

```
__fac.posm__
```

## Example: Factorial (tail recursive)

```
__tfac.posm__
```

## Dynamic code loading

```
__module_calls.posm__
```

## Example: Concurrent Ackermann

```
__ackermann.posm__
```

## Message passing

```
__message_passing.posm__
```
