# The R programming language

R is a tiny but powerful programming language


functional, dynamically typed and concurrency oriented
programming language.



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
class, interface
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

`big` : Arbitrary-precision integer

`float` : 32 or 64-bit floating point (depending on the target
architecture)

`char` : Unicode code point

## Composite types

`string` : An immutabe sequence of UTF-8 encoded characters

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

The following symbols are language-reserverd keywords and they cannot
be defined in user code:

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
case
default
match
timeout
class
new
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
symbol `this` denotes the current object inside a method's
definition. `self` denotes the job id which user code currently
executes in.

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

Raw strings are enclosed in double quotation as well but are prefixed
with the small letter `r`. No escape sequeces are meanigfull in raw
strings and are parsed verbatim.

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

Function literal definition follow the same syntax as regular
function definition except thatthe function name is missing.

Example:

```
sum = (x, y) { x + y }
a = sum(1, 2)                  // 3
```

# Operators

## No implicit numeric conversion

A a binary operator who operates on numerical values require the
operands to be of the same type. No implicit numeric conversion is
done.

Examples:

```
a = 3
b = 042
c = 93326215443944152681B
d = 3.0
e = a + cast(int)d * b             // 105
f = c / cast(big)d + cast(big)a    // 31108738481314713603B
g = d + c                          // Not possible!
```

The `typeof` operator can be used to check the type of a numerical:

```
a = 42
if (typeof(a) == Type.int) {
    a = 31108738481314713603B + cast(big)a
}
```

## Postfix expressions

The member access operator `a.b` accesses the member named `b` within
the object `a`.

## Increment and decrement

`++` and `--` work as in C and C++.

## Function call

`fun()` invokes the function `fun` with a comma separated argument
list of expressions. Arguments are evalulated left to right before the
function is is invoked. 'fub' can refer to the name of a defined
function or to a function literal.

## Indexing

The expression `arr[i]` access the i:th element of an array or
table. For an array `i` must be of an integral type and for a table it
can be of any type. If the indexing expression is an lvalue of an
assignment operator (`a[i] = 0`) the expression inserts an array in
the array or table.

## Array slices

If `arr[i]` is an array the expression `a[i .. j]` returns a slice
startinat index i and ending att index j - 1. No data is copied from
the origin array so if the slice is updated the origin array will be
updated too.

Examples:

```
a = [1, 2, 3, 4, 5]
b = a[1 .. 3]          // b = [2, 3]
c = a[2 .. $ - 1]      // c = [3, 4]
d = b ~ c              // d = [2, 3, 3, 4] (Copy)
d[1] = 42              // d = [2, 42, 3, 4]
a[2] = 23              // a = [1, 2, 23, 4, 5]
                       // b = [2, 23]
                       // c = [23, 4]
                       // d = [2, 42, 3, 4]
e = b.dup()            // Explicit copy
b = [4711] ~ b         // b = [4711, 2, 23] (Copy)
e ~= 4711              // e = [2, 23, 4711]
f = a[$ / 2 .. $]      // What do we get?
g = a
h = a.dup()
g is a                 // true
h is a                 // false
g == a                 // true
h == a                 // true
```

## A complete list of operators

The complete list of available operators can be found in the
expression precedence list in Appendix A.



Take a look in the precedence list in appendinx










# The main function

Nothing can be declared in the global context except for the `main`
function which **must** be declared there.



class, main, enum




```
main(_args) {
  "Hello World!"
}
```
## Comments

`//` and `/* ... */`




## Symbols






## String


String interpolation is supported.

Examples are interpolated maybe interpoao

```
a = 3.0
b = "foo $a is not ${a + 1.0}"   // foo 3.0 is not 4.0"`
```





Examples:

```
a = "foo"
b = "bar"
c = a ~ b               // c = "foobar" (Copy)
c = a ~ '\u03c9'        // c = "fooω" (Copy)
```


`string`     An immutable UTF-8 string

Examples:

`"foo"`

`r"foo\nbar"` is a raw string without escape processing

`"foo $a is not ${a + 1.0}"` becomes `"foo 3.0 is not 4.0"`

Examples:

```
a = "foo"
b = "bar"
c = a ~ b               // c = "foobar" (Copy)
c = a ~ '\u03c9'        // c = "fooω" (Copy)
```







## Dynamic arrays


Above we only examplify with arrays of integers but all available
types in R can be stored in dynamic arrays.

## Associative arrays

All keys and values may have any type:

```
[ "no" : 1.0 ]
```

Examples:

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

## Functions

Define a named function like this:

```
foo(a, b, c = 0) {
  c
  d
}

foo(a = 1) {
  a
}
```

Note how trailing parameters may have default values.

> [!NOTE]
> No support for currying and variadic parameters

Calling convention:

`foo(2, 6)`

or

`bar.foo(2, 6)`

if `foo` is implemented in module `bar`.

Functions may be nested:

```
foo(a, b, c = 0) {
  bar(d) {
    d
  }
  bar(a)
}
```

Define an anonymous function like this:

```
(a, b) {
    b
}
```

This is an example of a map function:

```
main() {
    l = [1, 2, 3]
    f = (l, n) { l[n] + 1 }
    true <~ map(l, f)
}

map(l, f, n = 0) {
    if (n > l.length()) {
        true
    } else {
         l[n] = f(l, n)
         a(l, f, n + 1)
    }
}
```

`<~` is used for matching. More on that below.

If a function parameter is pre-pended with a `ref` keyword it is
referred to by reference instead value. This only has meaning for
`int`, `float` and `bool` values. All other types are already referred
to by reference.

Example:

```
a = 1
foo(ref b) {
    b += 1
}
foo(a)
writeln(a)                  // 2
```

## Enums

```
enum Bonk {
  a,
  b,
  c
}
```

Used like this:

```
Bonk.a
Bonk.c
```

## Variables

```
aVariable = 1
B52 = "foo"
```

## Tuples

```
#(1, 2)
#(a, #(b, 4))
```

## Classes

```
class Foo {
  public a
  private b
  readonly c
  const d

  this(a, g) {          // Constructor
    this.a = a
    b = g
  }

  ~this(a, g) {         // Destructor
    this.a = a
    b = g
  }

  public foo() {
    0
  }

  private bar(b) {
    b
  }
}
```

> [!NOTE]
> No support for inheritance (see interface below) and classes can
> only be defined as top level constructs. By design.

A class Foo can be instantiated like this:

`foo = new Foo(1, 2)`

or

`foo = new bar:Foo(1, 2)`

if Foo is availble in module `bar` (read more aboy hierarchical
modules below). Access to member variables and functions look like
this:

```
foo.a
foo.a = 1
foo.bar(1)
```

A class may choose to implement a mandatory interface. The interface
defines which member variables and functions that must be provided by
the class. An interface definition looks like this:

```
interface Bar {
  public a
  public fn foo()
}
```

A class which decides to implement this interface looks like this:

```
class Foo : Bar, Bonk {
   // See Foo class above
}
```

Above `Foo` implements two interfaces `Bar` and `Bonk`.

A class can also be defined as a singleton:

```
singleton class Foo {
   // See Foo record above
}
```

It means what you think.

If you need to define a bunch of constants you typically do this in a
singleton class like this:

singleton class Math {
    const PI = 3.1
    const SQUARE2 = math.sqrt(2)
}

## Control flow

```
if (expr) {
  a
} else {
  b
  c
}
```

and

```
switch (expr) {
    case "foo": {
        1
    }
    default: {
        ss
    }
}
```

## Matching

Matching can be done with the `<~` operator.

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

## Macros

No macros

## Exceptions

No exceptions

## Hierarchical modules





```
import std@stdio : writeln
writeln("foo")
std@stdio$wrute
```


hmmmmmm


or explicitly:

`std@stdio@writeln("foo")`

Name conflicts between constructs between modules are detected whenever
they are used.

The `std.jobs` module contains the functionality needed to work with
concurrent jobs and message passing in between them, i.e. `spawn()`,
`send()` and `recv()` functions.






## Concurrency

Any function can be spawned to run as a concurrent job with the
`spawn` keyword:

`jid = spawn ackermann(3, 1)`

Jobs share **nothing** with other jobs and the input parameters are
automatically deep copied before job start:

```
a = [1, 2, 3]
jid = spawn sum(a)    // a.dup() is performed automatically
```

> [!NOTE]
> All jobs get their own copy of all singelton classes. Nothing must
> be shared.

`spawn` returns a job id (jid) which can be used to send messages to
the job with the `send` keyword:

`send jid #(timeout, 1000)`

A message sent to a job ends up in its mailbox and can be retrieved
with the `receive` keyword:

```
receive {
    case #(?jid, ?result): {
        writeln("Job $jid sent result $result")
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

Here the mailbox is restricted to at most 64 messages and if a
sending job hits this threshold it automatically blocks in `send`
until the mailbox contains less messages.

As an alternative to `OnCrowding.block` `OnCrowding.ignore` can be
used to specify that overflowing messages should be ignored. The
`OnCrowding` enum can alternatively be replaced with a function that
returns `false` if overflowing messages should be ignored or `true` if
`send` should continue to block.

The last concurrency keyword is `self`. It is a sibling to `this` in
classes, but it return the job id for the currently running job.

The `std.concurrency` module also contains these functions:

`monitor(jid)` : Send a message `#(JobMonitor.died, jid, reason)` to
me if the job dies.

`link(jid)` : Send a message `#(JobMonitor.died, jid, reason)` to me
if the job dies. Do the same to the linked job if I die.

The `spawn` keyword have the siblings `mspawn` and `lspawn` which spawn
jobs at the same time as they create a monitor, or a link, to the new
job. That didn't come as a surprise.

`kill(jid)`: Go figure

### A concurrency example

A small example may clear things up. Below is a main function which
spawns jobs to compute Ackermann function values for the parameters `m
= 3, n = 1 .. 10`. The `main` function uses an Ackermann singleton
class to start 10 jobs and then waits for all jobs to send a result
back as a message.

```
import std.concurrency
import std.stdio

main() {
  jids = Ackermann.startJobs(3, 10)
  Ackermann.waitForJobs(jids)
}

singleton class Ackermann {
    public startJobs(m, n, i = 0, jids = []) {
        if (i < n) {
            computeAckermann(fromJid, m, n) {
                result = ackermann(m, n)
                send fromJid #(self, result)
            }
            jid = mspawn computeAckermann(self, m, ++i)
            setMaxMailboxSize(jid, 4, OnCrowding.block)
            startJobs(m, n, i, jids ~ jid)
        }
        jids
    }

    public waitForJobs(jids) {
        if (jids.length > 0) {
            receive {
                case #(?jid, ?result): {
                    writeln("Compute job $jid sent us the result $result")
                }
                case #(JobMonitor.died, ?jid, ?reason): {
                    if (jids.member(jid)) {
                        writeln("Oh no! Compute job $jid died: $reason")
                    } else {
                        writeln("Oh no! Anyway...")
                }
            }
            waitForJobs(jids[0 .. $ - 1])
        }
    }

    private ackermann(m, n) {
        if (m == 0) {
            n + 1
        } else if (n == 0) {
            ackermann(m - 1, 1)
        } else {
            ackermann(m - 1, ackermann(m, n - 1))
        }
    }
}
```

# Appendix A: Expressions in decreasing order of precedence


``<symbol>

| Expression | Description                                                    |
|------------|----------------------------------------------------------------|
| <symbol>   |                                                                |
| this       | The current object inside a method                             |
| $          | Current array size (valid inside an index or slice expression) |
| null       | The null reference                                                               |
Get the TypeInfo object associated with T152.1.1 on page 31)

Boolean true (52.2.1 on page 32) Boolean false ($2.2.1 on page 32)

false

Numeric literal ($2.2.2 on page 32. $2.2.3 on page 333

Character literal ($2.2.4 on page 34)

string

String literal ($2.2.5 on page 35)

Array literal ($2.26 on page 39)

function

Function literal ($2.2.7 on page 40)

assert(a)

In debug mode, if a is not nonzero, halt program: in release mode.

do nothing (52.3.4.1 on page 46)

assert (a, b)

Same as above; make b part of the error information (§ 2.3.4.1 on

true

char

page 46)

mixin(a)

IsExpr

Mixin expression (52.3.4.2 on page 47)

is expression ($2.3.43 on page 48)
