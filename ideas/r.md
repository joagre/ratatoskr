# The R programming language

```
import std.stdio

main() {
  stdio.writeln("Hello World!")
}
```

R is a tiny but powerful programming language


functional, dynamically typed and concurrency oriented
programming language.

lokala context är bra




condynamically typed language with type inference and

Dynamically typed
Hindley-Milner type inference
Concurrently oriented
Time sliced light weight green threads scheduler(1024 bytes)
Pattern matching
Selective message passing using pattwern matching
Fixed and dynmaic arrays
Hashtables
Light-weight (microprocessor -> desktop)
Small memory foot print but built to scale
Pluggable Scceduler (build you own Interpreter and plug it in)
int, big, float, bool, tuple, fixed and dynamic array, hashtable, string,
struct, interface
Everything is
No data can be shared be between job + spawn, send , recieve, monitor,
link, telleub external format
 etc (no data can be shared)
Not immutable
Lean
and fast
Register machine
Functional, everythiung is an expression
Tail recusrve optimixation
Everything is refewred to be reference expecpt for int, float and long
which can be reference based
utf8 in strings

ows much to D, syntax and semantics

shamelessly taken the syntax whwre it fits and removed everything that
wqs necessaryu in this. just the fatct taht everyins aexpresion no
loop constructs.. no semicolon. no much

astdlib for string, lists, and network built

Structural and reference equality
Immutable
A script langue or not

No pointers
No type specifications
No exceptions
No preprocessor and no macros
No variadic functions
No mutexes
Not immutable
No inheritance (if that means not being object oriented it may be so)

Everything is an expression. no loopiing iteratiuons.

Syntactically a { language and ows D { based langiugae, ows heavy to the d language but because of its being a dynamic ally tyoped and garbage collected, functional (everything is an expression). No semicolons. Meager syntax but recognizable.


where everything is a an expressive loads of


with because the functional everything is an expression


# Types

## Basic types

`bool` : Boolean value `true` or `false`

`int` : Signed 32/64 bits integer (depending on the target
architecture)

`uint` : Unsigned 32/64 bits integer (depending on the target architecture)

`big` : Arbitrary-precision integer

`float` : 32 or 64-bit floating point (depending on the target
architecture)

`char` : Unicode code point

`function` : A reference toa  function

## Composite types

`string` : Immutabe sequence of UTF-8 encoded characters

`array`: Contigous region of memory containing values of any type

`table`: Table mapping between a key of any type and a value of any
type

`object` : Unit of encapsulation for constants, member variables and
member functions

# Symbols

A symbol is a case sensitive string of characters starting with a
ASCII letter or an underscore, followed by any number of ASCII
letters, underscores or digits.

`^[[:alpha:]_][[:alnum:]_]*$`.

## Special symbols

The following special symbols (keywords) are language-reserverd
keywords and they cannot be defined in user code:

```
import
true
false
null
enum
if
then
else
switch
default
match
timeout
struct
public
private
readonly
const
interface
singleton
this
spawn
mspawn
lspawn
send
receive
self
```

A few symbols are recognized as primitive expressions: The special
symbol `this` denotes the current object inside a struct method
definition, and `self` denotes the job which user code currently is
runs in.

# Literals

## Boolean literals

The `bool` literals are `true` or `false`.

## Integral literals

R supports decimal, octal, hexadecimal, binary and bignum literals. A
bignum literal is suffixed with the letter `b`.

Examples:

```
a = 4                 // int in decimal format
b = 017               // int in octal format
c = 0xffff            // int in hexadecimal format
d = 0b101010100       // int in binary format
e = 298347928347987b  // bignum
```

## Floating-point literals

Examples:

```
a = 1.0
b = .666e2
```

## Character literals

A character literal is a unicode code point enclosed within single
quotation marks. A unicode character takes up four bytes.

Examples:

```
a = 'A'
b = 'ω'
c = '\u03c9' // ω
```

## String literals

Quoted strings are sequences of UTF-8 encoded characters enclosed in
double quotation. Escape sequences are meaningfull in quoted strings.

Raw strings are enclosed in double quotation but are prefixed with the
letter `r`. No escape sequeces have meaniging in raw strings and are
parsed verbatim.

Examples:

```
a = "foo"
b = r"foo\nbar"  // b.length != 5
```

## Array literals

Array literals are represented as a comma-seprated sequence of values
enclosed in square brackets.

Examples:

`a = [ 3.14, "foo", 1816381276163287b ]`

## Table literals

Table literals are represented as a comma-seprated sequence of
key-values (separated by :) enclosed in square brackets.

Examples:

`a = [ "foo" : 12, 981237198192378b = 3.14 ]`

## Function literals

Function literals follow the same syntax as regular function
definitions except that the function name is missing.

Example:

```
sum = (x, y) { x + y }
a = sum(1, 2)                  // 3
```

# Operators

## No implicit numeric conversion

A binary operator which operates on numerical values requires the
operands to be of the same type. No implicit numeric conversion is
performed.

Examples:

```
a = 3
b = 042
c = 93326215443944152681B
d = 3.0
e = a + cast(int)d * b             // 105
f = c / cast(big)d + cast(big)a    // 31108738481314713603B
g = d + c                          // Produces a compiler error
```

The `typeof` operator can be used to check the type of a numerical:

```
import . std.type

a = 42
if typeof(a) == Type.int {
    a = 31108738481314713603B + cast(big)a
}
```

## Postfix expressions

The member access operator `a.b` accesses the member named `b` within
the object `a`.

## Function calls

`fun(a,b)` invokes the function `fun` with a comma separated argument
list of expressions. Arguments are evaluated left to right before the
function is invoked. 'fun' can refer to the name of a defined function
or a function literal.

## Indexing

The expression `arr[i]` access the i:th element of an array or
table. For an array `i` must be an integral type and for a table it
can be any type. If the indexing expression is an lvalue in an
assignment operator (`a[i] = 0`) the expression inserts a value in the
array or table.

## Array slices

The array expression `a[i .. j]` returns a slice which starts at index
i and ends with index j - 1. No data is copied from the origin array,
i.e. if the slice is updated the origin array will also be be updated.

# Comments

Everything after `//` and with `/* ... */` are considered to be
comments.

# Lexical scope

A compound statement is a sequence of expressions enclosed in curly
braces. Expressions there within are evaluated in sequence and the
braces introduces a lexical scope. A symbol defined in a scope is not
visible outside of the scope and it shadows a symbol with the same
name outside of the scope.

Example:

```
main() {
    a = 42
    {
        b  = a + 1
        a = b
    }
    // b not defined
    // a = 43
}
```

# The `if` expression

```
if a {
  b
} else {
  c
  d
}
```

An `if` expression returns the last expression in the evaluated
compound statement.

# The `switch` expression

```
switch a {
    b {
        42
    }
    c {
        a + 1
    }
    default {
        a
    }
}
```

A `switch` expression returns the last expression in the evaluated
compound statement. There is no fall through mechanism and `default`
case is optional.

# Tuples

Tuples are a finite ordered sequence of elements. It is a data
structure that can hold a fixed number of elements of any type. The
elements of a tuple are usually accessed using the match operator
`<~`. Tuples comma separated values within paranthesis prefixed with
`#`.

Example:

```
a = 42
b = #(4711, #(a, [1, 2]))
#(_, #(_, [_, c])) = b    // c = 2
```

# Arrays

Arrays are contiguous regions of memory containg elements of any
type. Arrays support in-memory slicing that allows yout to select and
work with only a portion of an array.

Examples:

```
a = [1, 2, 3, 4, 5]
b = a[1 .. 3]          // b = [2, 3]
c = a[2 .. $ - 1]      // c = [3, 4]
d = b ~ c              // d = [2, 3, 3, 4] (A copy!)
d[1] = 42              // d = [2, 42, 3, 4]
a[2] = 23              // a = [1, 2, 23, 4, 5]
                       // b = [2, 23]
                       // c = [23, 4]
                       // d = [2, 42, 3, 4]
e = b.dup()            // An explicit copy
b = [4711] ~ b         // b = [4711, 2, 23] (A copy!)
e ~= 4711              // e = [2, 23, 4711]
f = a[$ / 2 .. $]      // What do we get?
g = a
h = a.dup()
h.length == 5          // true
g is a                 // true
h is a                 // false
h !is a                // true
g == a                 // true
h == a                 // true
h != a                 // false
h.length = 4           // h = [1, 2, 23, 4]
```

# Tables




```
a = [ "a" : 1.0, "b" : "foo" ]
a["a"] = "bar"
a[42] = "baz"          // a = ["a" : "bar", "b" : "foo", 42 : "baz"]
b = a                  // b = ["a" : "bar", "b" : 0, 42 : "baz"]
b["a"] = 0             // a = ["a" : 0, "b" : 0, 42 : "baz"]
                        // b = ["a" : 0, "b" : 0, 42 : "baz"]
c = a["a"]'             // c = 0
d = a ~ [42 : 4711]    // d = [42 : 4711, "a" : 0, "b" : 0] (Copy)
e = b.dup()            // Explicit copy
f = b
f is b                 // true
e is b                 // false
f == b                 // true
e == b                 // true
```

> [!NOTE]
> Structural equality is used for all key value lookups (even for
> class instances)














# The `match` expression











## Matching

Deconstructing of all types of data can be done using the match
operator `<~`.

Examples:

```
a = 1
#(a, ?a, 1) <~ #(1, 2, 1)                    // a = 2
#(?a, b, ?h) <~ foo(42)
[1, ?a] <~ [1, 2]                            // a = 2
[42 : 1, "foo" : ?a] <~ [42 : 1, "foo" : 2]  // a = 2
```

`?` introduces an unbound variable.






Matching can also be done like this:

```
match (expr) {
  case match-expr: {
    a
    b
  }
  case match-expr: {
    c
  }
}
```

Example:

```
a = 1
b = 3
match (expr) {
  case #(1, ?a): {
    a
  }
  case a || b: {
    a + 1
  }
  case _: {
    0
  }
}
```

`_` is a wildcard and `||` introduces an `or` pattern.








#(c, [_, <~ #(a, #(b, [1, 2]))

```



Example:

```




#(a, #(b, 4))  // A nested tuple




unpacking or indexing. Tuples are immutable in some languages, meaning
that once a tuple is created, its size and the elements it contains
cannot be changed.

Tuples are fixed sized

```








# Concurrency

Any function can be spawned to run as a concurrent job with the
`spawn` keyword:

`jid = spawn ackermann(3, 1)`

Jobs share **nothing** with other jobs and input parameters are
automatically deep copied before job starts:

```
a = [1, 2, 3]
jid = spawn sum(a)    // a.dup() is performed automatically
```

> [!NOTE]
> If any singleton structes (see below) have been defined each job gets
> its own own copy of it. Nothing is shared between jobs.

`spawn` returns a job id (jid) which can be used to send messages to
job with the `send` keyword:

`send jid #(timeout, 1000)`

A message sent to a job ends up in its mailbox and can be retrieved
with the `receive` keyword:

```
receive {
    #(?jid, ?result) {
        stdio.writeln("Job $jid sent result $result")
        result
    }
    timeout 1000 {
        42
    }
}
```

(the `timeout` keyword is optional)

The mailbox is unbounded in size but can be restricted using the
`setMaxMailboxSize` function provided by the `std.concurrency` module:

`setMaxMailboxSize(jid, 64, OnCrowding.block)`

Above a job's mailbox is restricted to contain at most 64 messages,
and if a sending job hits this threshold it is automatically blocked
in `send` waiting for the mailbox contain less messages.

`OnCrowding.ignore` can be used instead `OnCrowding.block` to specify
that overflowing messages should be ignored. The `OnCrowding` enum can
alternatively be replaced with a function that returns `false` if
overflowing messages should be ignored or `true` if the sending job
should be blocked in `send`.

The last concurrency keyword is `self` and it refers to the job which
user code currently runs in.

The `std.concurrency` module also provides these functions:

`monitor(jid)` : Send a message `#(JobMonitor.died, jid, reason)` to
me if a job dies

`link(jid)` : Send a message `#(JobMonitor.died, jid, reason)` to me
if a job dies. Do the same to the linked job if I die.

The `spawn` keyword have the siblings `mspawn` and `lspawn`. They,
respectively, spawn jobs at the same time as they create a monitor, or
a link. That didn't come as a surprise.

`kill(jid)`: Just like that

## A concurrency example

A small concurrent example may clear things up. Below is a main
function which spawns jobs to compute Ackermann function values for
the parameters `m = 3, n = 1 .. 10`. The `main` function uses an
Ackermann singleton struct to start 10 jobs and then waits for all jobs
to send a result back as a message.

```
import conc = std.concurrency
import std.stdio

main() {
  jids = Ackermann.startJobs(3, 10)
  Ackermann.waitForJobs(jids)
}

singleton struct Ackermann {
    public startJobs(m, n, i = 0, jids = []) {
        if i < n {
            computeAckermann(fromJid, m, n) {
                result = ackermann(m, n)
                send fromJid #(self, result)
            }
            jid = mspawn computeAckermann(self, m, ++i)
            conc.setMaxMailboxSize(jid, 4, OnCrowding.block)
            startJobs(m, n, i, jids ~ jid)
        }
        jids
    }

    public waitForJobs(jids) {
        if jids.length > 0 {
            receive {
                #(?jid, ?result) {
                    stdio.writeln("Compute job $jid sent us the result $result")
                }
                #(JobMonitor.died, ?jid, ?reason) {
                    if jids.member(jid) {
                        stdio.writeln("Oh no! Compute job $jid died: $reason")
                    } else {
                        stdio.writeln("Oh no! Anyway...")
                }
            }
            waitForJobs(jids[0 .. $ - 1])
        }
    }

    private ackermann(m, n) {
        if m == 0 {
            n + 1
        } else if n == 0 {
            ackermann(m - 1, 1)
        } else {
            ackermann(m - 1, ackermann(m, n - 1))
        }
    }
}
```

# Appendix A: Expressions in decreasing order of precedence

Everything is an expression.

> [!NOTE]
> `import`, `enum`, and `struct` can only be used as top level constructs in a module.

| Expression    | Description                                                    |
|---------------|----------------------------------------------------------------|
| <symbol>      |                                                                |
| this          | The current object inside a struct method                      |
| self          | The job which user code currently runs in                      |
| $             | Current array size (valid inside an index or slice expression) |
| null          | The null reference                                             |
| true          |                                                                |
| false         |                                                                |
| &lt;bool>     |                                                                |
| &lt;int>      |                                                                |
| &lt;big>      |                                                                |
| &lt;float>    |                                                                |
| &lt;char>     |                                                                |
| &lt;string>   |                                                                |
| &lt;array>    |                                                                |
| &lt;table>    |                                                                |
| &lt;function> | (a, b) { c }                                                   |
| ( a )         | Paranthesized expression                                       |
| a.b           | Member access                                                  |
| a++           |                                                                |
| a--           |                                                                |
| a(b, c = d)   | Function call                                                  |
| a[i]          |                                                                |
| a[b .. c]     |                                                                |
| ++a           |                                                                |
| --a           |                                                                |
| -a            |                                                                |
| +a            |                                                                |
| !a            |                                                                |
| ~             | Bitwise complement                                             |
| cast(t)a      | Cast expression                                                |
| a ^^ b        | Exponentiation                                                 |
| a * b         |                                                                |
| a / b         |                                                                |
| a % b         | Modulus                                                        |
| a + b         |                                                                |
| a - b         |                                                                |
| a ~ b         | Concatenation                                                  |
| a &lt;&lt; b  |                                                                |
| a >> b        |                                                                |
| a in b        | Table membership                                               |
| a == b        | Equality test (a == b == c is not legal)                       |
| a != b        |                                                                |
| a is b        | Identity test                                                  |
| a !is b       | !(a is b)                                                      |
| a &lt; b      |                                                                |
| a &lt;= b     |                                                                |
| a > b         |                                                                |
| a >= b        |                                                                |
| a \| b        |                                                                |
| a ^ b         | Bitwise xor                                                    |
| a & b         |                                                                |
| a && b        | Logical and                                                    |
| a \|\| b      |                                                                |
| a = b         |                                                                |
| a += b        | In-place add                                                   |
| a -= b        |                                                                |
| a *= b        |                                                                |
| a /= b        |                                                                |
| a %= b        |                                                                |
| a &= b        |                                                                |
| a \|= b       |                                                                |
| a ^= b        |                                                                |
| a ~= b        | In-place concatenation                                         |
| a &lt;~ b     | Matching                                                       |
| a &lt;&lt;= b |                                                                |
| a >>= b       |                                                                |
| const         |                                                                |
| public        |                                                                |
| private       |                                                                |
| readonly      |                                                                |
| if            |                                                                |
| else          |                                                                |
| switch        |                                                                |
| default       |                                                                |
| spawn         |                                                                |
| mspawn        |                                                                |
| lspawn        |                                                                |
| self          |                                                                |
| send          |                                                                |
| receive       |                                                                |
| struct        |                                                                |
| interface     |                                                                |
| singleton     |                                                                |
| match         |                                                                |
| timeout       |                                                                |
| enum          |                                                                |
| a(b, c) { d } | Named function definition                                      |
