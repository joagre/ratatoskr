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


Inspired D and Erlang

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
int,  float, bool, tuple, fixed and dynamic array, hashtable, string,
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

# Overall structure


import std.concurrency
import std.stdio

enum

struct



fn main() {
  jobs = Ackermann.startJobs(3, 10);
  Ackermann.waitForJobs(jobs);
}

singleton Ackermann {
    public fn startJobs(m, n, i = 0, jobs = []) {
        if i < n {
            fn computeAckermann(parentJob, m, n) {
                result = ackermann(m, n);
                 parentJob <: #(self, m, n, result);
            }
            job = mspawn computeAckermann(self, m, ++i);
            concurrency.setMaxMailboxSize(job, 4, concurrency.OnCrowding.block);
            startJobs(m, n, i, jobs ~ job);
        }
        jobs;
    }

    public fn waitForJobs(jobs) {
        if jobs.length > 0 {
            receive {
                case #(?job, ?m, ?n, ?result) {
                    stdio.writeln("ackermann($m, $n) = $result");
                }
                case #(JobMonitor.died, ?job, ?reason) {
                    stdio.writeln("Oh no! Compute job $job died: $reason");
                }
            }
            waitForJobs(jobs[0 .. $ - 1]);
        }
    }

    private fn ackermann(m, n) {
        if m == 0 {
            n + 1;
        } else if n == 0 {
            ackermann(m - 1, 1);
        } else {
            ackermann(m - 1, ackermann(m, n - 1));
        }
    }
}





# Types

## Basic types

`bool` : Boolean value `true` or `false`

`int` : Signed 64-bits integer (if it's smaller
-1,152,921,504,606,846,976 or larger than 1,152,921,504,606,846,975
it's automatically represented as an arbitrary-precision integer under
the hood

`float` : 64-bits floating point

`char` : 32-bits Unicode code point

`function` : Function reference

`job` : Job reference

`enum`: Enum reference

FIXME: Add a chapter about Enums

## Composite types

`string` : Immutable sequence of UTF-8 encoded characters

`array`: Contiguous region of memory which can contain values of any type

`map`: A mapping between a key of any type and a value of any type

`struct` : Unit of encapsulation for member variables and functions

## Type management

All expressions can be type checked in run-time using the functions
`isBool`, `isInt`, `isFloat`, `isChar`, `isFunction`, `isJob`,
`isEnum`, `isString`, `isArray`, `isTinyArray`, `isSmallArray`,
`isMedium`, `isMap`, `isStruct` and `typeof()`.

Example:

```
a = 3.14;
a.isInt();          // false
a.isFloat();        // true
a.typeof();         // "float"
```

Binary operators which operates on `int` and `float` values require
their operands to be of the same type. No implicit numeric conversion
is performed. The `cast` operator only exists to cast between `int`
and `float` values.

Example:

```
a = 3;
b = 93326215443944152681;
c = 3.0;
d = b / cast(int)c + a;    // 31108738481314713603
e = d + c;                 // A compiler error!
```

The `inspect()` function provides more information about types.

Example:

```
Enum Foo {       // Defined in bar.sa
  a = 3.14,
  b
}

c = Foo.a;
d = [1, "foo"];
c.inspect();     // [ "type" : "enum",
                 //   "name": "a",
                 //   "value": 3.14,
                 //   "owner" : #("bar", "Foo") ]
d.inspect();     // [ "type" : "array", "length": 2 ]
```

> [!NOTE]
> The enumeration value is normally accessed with `c.getValue()` and
> the array length with `d.length`

# Symbols

A symbol is a case sensitive string of characters starting with an
ASCII letter or an underscore, followed by any number of ASCII
letters, underscores or digits.

`^[[:alpha:]_][[:alnum:]_]*$`.

Keywords, variables, functions, structs, enums are all symbols.

*Consideration: I may support UTF-8 not only ins strings*

## Keywords

The following 28 special symbols are language-reserved keywords and
they cannot be defined in user code:

```
import
true
false
enum
ref
in
is
fn
if
then
else
switch
default
match
struct
singleton
interface
public
private
readonly
const
this
spawn
mspawn
lspawn
receive
timeout
self
```

# Literals

## Boolean literals

The `bool` literals are `true` or `false`.

## Integral literals

Integrals can be formatted as decimal, octal, hexadecimal literals.

Examples:

```
a = 4;                 // Integer in decimal format
b = 017;               // Integer in octal format
c = 0xffff;            // Integer in hexadecimal format
d = 0b101010100'       // Integer in binary format
```

## Floating-point literals

Examples:

```
a = 1.0;
b = .666e2;
```

## Character literals

A character literal is a Unicode code point enclosed within single
quotation marks. A Unicode character consists of four bytes under the
hood.

Examples:

```
a = 'A';
b = 'ω';
c = '\u03c9'; // ω
```

## String literals

Quoted strings are sequences of UTF-8 encoded characters enclosed in
double quotation marks. Escape sequences are meaningful in quoted
strings.

Raw strings are enclosed in double quotation marks but are prefixed
with the letter `r`. No escape sequences have meaning in raw strings
and are parsed verbatim.

Examples:

```
a = "foo"
b = r"foo\nbar"  // b.length != 7
```

## Array literals

Array literals are represented as a comma-separated sequence of values
of any type enclosed in square brackets.

Examples:

`a = [ 3.14, "foo", 1816381276163287b ];`

## Map literals

Map literals are represented as a comma-separated sequence of
key-values (separated by a `:` character) enclosed in square
brackets. Keys and values may be of any type.

Examples:

`a = [ "foo" : 12, 981237198192378b = 3.14 ];`

## Function literals

Function literals follow the same syntax as regular function
definitions (see below) except but with an ommitted function name.

Example:

```
sum = fn (x, y) { x + y };
a = sum(1, 2);
```

# Operators

## Function calls

`fun(a, b)` invokes the function `fun` with a comma separated argument
list of expressions. Arguments are evaluated left to right before the
function is invoked. `fun` can refer to the name of a defined function
or a function literal.

## Indexing in arrays and maps

`arr[i]` access the i:th element of an array or map. For an array
`i` must be an integral type and for a map it can be a key of any
type. If the indexing expression is an `lvalue` in an assignment the
expression inserts a value in the array or map.

Example:

`a[i] = 0;`

## Array slices

`a[i .. j]` returns an array slice which starts at index `i` and ends
with index `j - 1`. No data is copied from the origin array, i.e. if the
slice is updated the origin array will also be be updated. `i` and `j`
can any valid expressions and the keyword `$` is the length of the
array.

# Constructs

## Comments

Everything after `//` and within `/* ... */` are considered comments.

## Lexical scope

A block sequence is a sequence of base expressions and
match/assignment expressions enclosed in curly braces. Expressions
there within are evaluated in sequence and the braces introduce a
lexical scope. A symbol defined in a scope is not visible outside of
the scope and it shadows a symbol with the same name being introduced
outside of the scope.

Example:

```
main() {
    a = 42;
    {
        b  = a + 1;
        a = b;
    }
    // b not defined
    // a = 43
}
```

## `if`

Example:

```
if a {
  b;
} else {
  c;
  d;
}
```

An `if` expression returns the last expression in the evaluated
block sequence.

## `switch`

Example:

```
switch a {
    case b {
        42;
    }
    case c {
        a + 1;
    }
    default {
        a;
    }
}
```

A `switch` expression returns the last expression in the evaluated
block sequence. There is no fall through mechanism and the
`default` keyword is optional.

## Tuples

Tuples are a finite ordered sequence of elements of any type. It can
hold a fixed number of elements. The elements of a tuple are usually
deconstructed with the `<*` match operator. Tuples are comma separated
values within parenthesis prefixed with `#`.

Example:

```
a = 42;
b = #(4711, #(a, [1, 2]));
#(_, #(_, [_, c])) <* b;    // c = 2
```

> [!NOTE]
> The `<*` match operator is described below

## Arrays

Arrays are contiguous regions of memory containing elements of any
type. Arrays support in-memory slicing which makes it possible to
select and work with portions of an array without copying.

Examples:

```
a = []                 // An empty array
a = [1, 2, 3, 4, 5]
b = a[1 .. 3]          // b = [2, 3]
c = a[2 .. $ - 1]      // c = [3, 4]
d = b ~ c              // d = [2, 3, 3, 4] (A copy!)
d[1] = 42              // d = [2, 42, 3, 4]
a[2] = 23              // a = [1, 2, 23, 4, 5]
                       // b = [2, 23]
                       // c = [23, 4]
                       // d = [2, 42, 3, 4]
e = b.dup()            // e = [2, 23] (A copy!)
b = [4711] ~ b         // b = [4711, 2, 23] (A copy!)
e ~= 4711              // e = [2, 23, 4711]
f = a[$ / 2 .. $]      // f = [23, 4, 5]
g = a                  // g = [1, 2, 23, 4, 5]
h = a.dup()            // h = [1, 2, 23, 4, 5] (A copy!)
h.length == 5          // true
g is a                 // true
h is a                 // false
h !is a                // true
g == a                 // true
h == a                 // true
h != a                 // false
h.length = 4           // h = [1, 2, 23, 4]
```

## Maps

An array can be seen as a function mapping between integers and values
of any type using an underlying contiguous memory region. A map is a
generalized array where key values of any type map to values of any
type.

Examples:

```
a = [:]                // En empty map
a = [ "a" : 1.0, "b" : "foo" ]
a["a"] = "bar"
a[42] = "baz"          // a = ["a" : "bar", "b" : "foo", 42 : "baz"]
b = a                  // b = ["a" : "bar", "b" : 0, 42 : "baz"]
b["a"] = 0             // a = ["a" : 0, "b" : 0, 42 : "baz"]
                       // b = ["a" : 0, "b" : 0, 42 : "baz"]
c = a["a"]             // c = 0
d = a ~ [42 : 4711]    // d = [42 : 4711, "a" : 0, "b" : 0] (A copy!)
e = b.dup()            // e = ["a" : 0, "b" : 0, 42 : "baz"] (A copy!)
e.length == 3          // true
f = b                  // f = ["a" : 0, "b" : 0, 42 : "baz"]
f is b                 // true
e is b                 // false
e !is b                // true
f == b                 // true
e == b                 // true
e != b                 // false
e.remove("b")          // e = ["a" : 0, 42 : "baz"]
e.keys ==
    ["a", 42] ||
    [42, 42]           // true
e.values ==
    [0, "baz"] ||
    ["baz", 0]         // true
```

> [!NOTE]
> Structural equality is used for all key lookups

## Strings

Strings are immutable sequences of UTF-8 encoded characters. String
interpolation is supported as well as random access. A character in a
string can be compared with a `char` value even though the
representation of characters in a string is UTF-8 encoded.

Examples:

```
a = 3.0
b = "foo $a is not ${a + 1.0}"  // String interpolation: b = "foo 3.0 is not 4.0"
a = "foo"
b = "bar"
c = a ~ b                       // String concatenation: c = "foobar" (A copy!)
c = a ~ '\u03c9'                // Character appending: c = "fooω" (A copy!)
c[3] == 'ω'                     // true
r"foo\nbar"                     // Raw string without escape processing
r.length == 9                   // true
```

## Functions

No function can be declared in the global context except for the `main`
function and it **must** be declared there. At most one main function
can be defined for each application.

```
import std.stdio

fn main() {
  stdio.writeln("Hello World!");
}
```

Functions can be overloaded and are defined like this:

```
fn foo(a, b, c = 0) {
  c;
  d;
}

fn foo(a = 1) {
  a;
}
```

Trailing parameters may have default values and these parameters can
be omitted in function calls. A function call can either be called
with positional parameters **only** or with named parameters
**only**. The following function calls are equivalent:

```
fn foo(2, 6);
fn foo(2, 6, 0);
fn foo(a = 2, b = 6);
fn foo(a = 2, b = 6, 0);
fn foo(b = 6, a = 2);
fn foo(b = 6, a = 2, 0);
```

Named functions can be defined within functions:

```
fn foo(a, b, c = 0) {
  fn bar(d) {
    d;
  }
  bar(a);
}
```

Anonymous functions are defined as described above under "Function
literals", i.e.

```
c = fn (a, b) {
        b;
    }
```

Example:

```
fn main() {
    l = [1, 2, 3];
    f = (l, n) { l[n] + 1 };
    map(l, f);
}

fn map(l, f, n = 0) {
    if (n > l.length()) {
        true;
    } else {
         l[n] = f(l, n);
         a(l, f, n + 1);
    }
}
```

If a function parameter is prepended with a `ref` keyword it is
referred to by reference instead of by value. This only has meaning
for the basic types, i.e. `bool`, `int`, `float`, `char`, `function`
and `enum`.

Example:

```
a = 1;
c = 2;
fn foo(ref b, ref c) {
    b += 1;
    c = fn (n) { n + 1};
}
foo(a);
writeln(a);                  // 2
c(2);                        // 3
```

## Matching and deconstruction

### `match`

```
match expr {
  match-expr {
    a
    b
  }
  match-expr {
    c
  }
}
```

The `match` keyword applies matching and optional deconstructing for
each `match-expr`. A `match-expr` can be any valid literal except it
may contain both bound and unbound variables (prefixed with `?`) and
wildcards `_`.

Example:

```
a = 1;
b = 3;
match (expr) {
  case #(_, ?a): {
    a;
  }
  case a || b: {
    a + 1;
  }
  case _: {
    0;
  }
}
```

## The `<*` operator

Matching/deconstructing can also be performed with the `<*` operator.

Examples:

```
a = 1;
#(a, ?a, 1) <* #(1, 2, 1);                   // a = 2
#(?a, b, ?h) <* foo(42);
[1, ?a] <* [1, 2];                           // a = 2
[42 : 1, "foo" : ?a] <* [42 : 1, "foo" : 2]; // a = 2
```

# Hierarchical packages

A satie file is called a module and it can be a member of a package. A
package is a directory in a hierarchy of nested package directories,
and each package directory contains zero or many modules.

Example:

```
${SPATH}/foo/
         f.sa
         bonk/zap/
              a.sa
              b.sa
         baz/honk/
             a.sa
             c.sa
```

The mpdule name is the filename with prefix.

If a module needs an enumeration defined in another module it imports
it and use it's base name to refer to the enumeration.

Example:

```
import foo.bonk.zap.a

main() {
  a.Color foo = a.Color.red;
}
```

Modules can also be imported using wildcard notation:

`import foo.bonk.zap.*`

If two modules share the same base name in two different packages one
of them must be aliased.

Example:

```
import foo.bonk.zap.a;
import honkA = foo.boz.honk.a;

main() {
  a.Color foo = a.Color.red;
  honkA.Color bar = honkA.Color.blue;
}
```

The alias `.` means that the need to prefix the enumeration with the
module name goes away.

Example:

```
import . = foo.bonk.zap.a

main() {
  Color foo = Color.red;
}
```

> [!NOTE] Name clashes only occur if a module refers to something that
> can't be uniquely resolved in compile time

# `enum`

Structs encapsulate member values and member functions and they can
only be defined on the top-level of each satie file.


FIXME

# `struct`

Structs encapsulate member values and member functions and they can
only be defined on the top-level construct of each module.

Examples:

```
struct Foo {
  public a = 1,
  private b = 2,
  public readonly c = 3,
  public const d = 4,
  private const e = 5,

  this(a, g) {          // Optional constructor
    this.a = a;
    b = g;
  }

  ~this(a, g) {         // Optional destructor
    this.a = a;
    b = g;
  }

  public fn foo() {
    a + b;
  }

  private fn bar(b) {
    c + d
  }
}
```

A struct Foo can be instantiated like this:

```
a = struct Foo
b = struct Foo(2, 1)
```

A struct may choose to implement mandatory interfaces. An interface
defines which member variables and functions that must be provided by
the struct. An interface definition looks like this:

```
interface Bar {
  public fn bonk()
}
```

A struct which decides to implement this interface looks looks like this:

```
struct Foo : Bar {
    public fn bonk() {
        0;
    }
    ...
}
```

A struct can implement several interfaces using a comma separated
sequence of interfaces.

```
struct Foo : Bar, Bonk {
    public fn bonk() {
        0;
    }
    ...
}
```

A struct can also be defined as a singleton. It means what you think.

If you need to define a bunch of constants you do this in a singleton
struct:

```
singleton struct Math {
    const PI = 3.1;
    const SQUARE2 = math.sqrt(2);
}
```

FIXME: interface

# Concurrency

Any function can be spawned to run as a concurrent job with the
`spawn` keyword:

`job = spawn ackermann(3, 1);`

Jobs share **nothing** with each other and input parameters to `spawn`
are automatically deep copied before the job is spawned:

```
a = [1, 2, 3];
job = spawn sum(a);    // a.dup() is performed automatically
```

> [!NOTE]
> If any singletons (see below) have been defined each job gets
> its own own copy of them. Nothing is shared between jobs.

`spawn` returns a job reference which, for example, can be used to
send messages to the job using the `<:` operator:

`job <: `#(timeout, 1000)`

A message sent to a job ends up in its mailbox and can be retrieved
with the `receive` keyword:

```
receive {
    case #(?job, ?result) {
        stdio.writeln("Job $job sent result $result")
        result
    }
    timeout 1000 {
        42
    }
}
```

(the `timeout` keyword is optional)

The mailbox is unbounded in size but can be restricted using the
`setMaxMailboxSize` function provided by the `std.concurrency`

Example:

`setMaxMailboxSize(job, 64, OnCrowding.block)`

Above a job's mailbox is restricted to contain at most 64 messages,
and if a sending job hits this threshold it is automatically blocked
in `<:` waiting for the mailbox to shrink.

`OnCrowding.ignore` can be used instead `OnCrowding.block` to specify
that overflowing messages should be ignored. The `OnCrowding` enum can
alternatively be replaced with a function that returns `false` if
overflowing messages should be ignored or `true` if the sending job
should be blocked in `<:`.

The last concurrency keyword is `self` and it refers to the job which
user code currently runs in.

The `std.concurrency` module also provides these functions:

`monitor(job)` : Send a message `#(JobMonitor.died, job, reason)` to
me if a job dies

`link(job)` : Send a message `#(JobMonitor.died, job, reason)` to me
if a job dies. Do the same to the linked job if I die.

The `spawn` keyword have the siblings `mspawn` and `lspawn`. They,
respectively, spawn jobs at the same time as they create a monitor, or
a link. That didn't come as a surprise.

`kill(job)`: Just like that

## A concurrency example

A small concurrent example may clear things up. Below is a main
function which spawns jobs to compute Ackermann function values for
the parameters `m = 3, n = 1 .. 10`. The `main` function uses an
Ackermann singleton to start 10 jobs and then waits for all jobs to
send a result back as a message.

```
import std.concurrency
import std.stdio

fn main() {
  jobs = Ackermann.startJobs(3, 10);
  Ackermann.waitForJobs(jobs);
}

singleton Ackermann {
    public fn startJobs(m, n, i = 0, jobs = []) {
        if i < n {
            fn computeAckermann(parentJob, m, n) {
                result = ackermann(m, n);
                 parentJob <: #(self, m, n, result);
            }
            job = mspawn computeAckermann(self, m, ++i);
            concurrency.setMaxMailboxSize(job, 4, concurrency.OnCrowding.block);
            startJobs(m, n, i, jobs ~ job);
        }
        jobs;
    }

    public fn waitForJobs(jobs) {
        if jobs.length > 0 {
            receive {
                case #(?job, ?m, ?n, ?result) {
                    stdio.writeln("ackermann($m, $n) = $result");
                }
                case #(JobMonitor.died, ?job, ?reason) {
                    stdio.writeln("Oh no! Compute job $job died: $reason");
                }
            }
            waitForJobs(jobs[0 .. $ - 1]);
        }
    }

    private fn ackermann(m, n) {
        if m == 0 {
            n + 1;
        } else if n == 0 {
            ackermann(m - 1, 1);
        } else {
            ackermann(m - 1, ackermann(m, n - 1));
        }
    }
}
```

# Appendix A: Expressions in decreasing order of precedence

Everything is an expression.

> [!NOTE]
> The keywords `import`, `enum`, `interface`, `struct` and `singleton`
> can only be used as top-level constructs.

| Expression      | Description                              |
|-----------------|------------------------------------------|
| (a)             | Paranthesized expression                 |
| {a, b}          | Sequence of expression                   |
| a.b             | Member access                            |
| a++             |                                          |
| a--             |                                          |
| a(b, c)         | Function call operator                   |
| a[i]            | Indexing operator                        |
| a[b .. c]       | Array slicing operator                   |
| ++a             |                                          |
| --a             |                                          |
| -a              |                                          |
| +a              |                                          |
| !a          B   |                                          |
| ~a              | Bitwise complement                       |
| cast(t)a        | Cast expression                          |
| a ^^ b          | Exponentiation                           |
| a * b           |                                          |
| a / b           |                                          |
| a % b           | Modulus                                  |
| a + b           |                                          |
| a - b           |                                          |
| a ~ b           | Concatenation                            |
| a &lt;&lt; b    |                                          |
| a >> b          |                                          |
| a >>> b         | Unsigned right shift                     |
| a in b          | Map membership                           |
| a == b          | Equality test (a == b == c is not legal) |
| a != b          |                                          |
| a is b          | Identity test                            |
| a !is b         | !(a is b)                                |
| a &lt; b        |                                          |
| a &lt;= b       |                                          |
| a > b           |                                          |
| a >= b          |                                          |
| a \| b          |                                          |
| a ^ b           | Bitwise xor                              |
| a & b           |                                          |
| a && b        B | Logical and                              |
| a \|\| b      B |                                          |
| a = b           |                                          |
| a &lt;~ b       | Matching                                 |
| a += b          | In-place add                             |
| a -= b          |                                          |
| a *= b          |                                          |
| a /= b          |                                          |
| a %= b          |                                          |
| a &= b          |                                          |
| a \|= b         |                                          |
| a ^= b          | In-place xor                             |
| a ~= b          | In-place concatenation                   |
| a &lt;&lt;= b   |                                          |
| a >>= b         |                                          |
| a >>>= b        | In-place unsigned right shift            |
| if            B |                                          |
| switch          |                                          |
| match           |                                          |
| receive         |                                          |

# Appendix B: PEG grammar

```
#
# Top level structure
#

Program            <- _ Import* _
                      (TopLevelConstructs _)* _
                      MainFunctionDef? _
                      (TopLevelConstructs _)*
                      EOF

Import             <- "import" __ (Identifier _ "=" _)? _ ModulePath
ModulePath         <- Identifier ("." Identifier)* ("." "*")?

TopLevelConstructs <- StructDef / InterfaceDef / EnumDef

#
# Expressions
#

Expr               <- Or

Or                 <- And (_ "||" _ And)*
And                <- Add (_ "&&" _ Add)*
Add                <- Multiplicate (_ ("+" / "-") _ Multiplicate)*
Multiplicate       <- Not (_ ("*" / "/") _  Not)*
Not                <- "!" _ Unary / Unary
Unary              <- ("+" / "-")? Primary
Primary            <- Boolean / Number / String / Character / If / Switch /
                      Match / Receive / Block / Tuple / Array / Table / Struct /
                      Variable / UnboundVariable / AnonFunctionDef /
                      FunctionCall / ParanthesizedExpr

Boolean            <- "true" / "false"

Number             <- Integral / FloatingPoint
Integral           <- OctalIntegral / HexIntegral / BinaryIntegral /
                      DecimalIntegral
OctalIntegral      <- "0"[0-7]+
HexIntegral        <- "0x"[0-9a-fA-F]+
BinaryIntegral     <- "0b"[01]+
DecimalIntegral    <- [0-9]+ !("." / [eE])
FloatingPoint      <- [0-9]* "." [0-9]+ ExponentPart? / [0-9]+ ExponentPart
ExponentPart       <- [eE] [+-]? [0-9]+

String             <- RegularString / RawString
RegularString      <- '"' ( EscapeSequence / [^"] )* '"'
EscapeSequence     <- "\\" [btnvfr"\\]
RawString          <- 'r"' [^"]* '"'

Character          <- "'" ( Escape / NonQuoteChar ) "'"
Escape             <- "\\" ( [abfnrtv'"\\] /
                      "x" HexDigit HexDigit /
                      "u" HexDigit HexDigit HexDigit HexDigit /
                      "U" HexDigit HexDigit HexDigit HexDigit
                          HexDigit HexDigit HexDigit HexDigit /
                      OctalDigit /
                      OctalDigit OctalDigit /
                      OctalDigit OctalDigit OctalDigit )
HexDigit           <- [0-9a-fA-F]
OctalDigit         <- [0-7]
NonQuoteChar       <- [^']

If                 <- "if" __ Expr _ Block
                      (_ "elseif" __ Expr _ Block)*
                      (_ "else" _ Block)?

Switch             <- "switch" __ Expr _ "{"
                      (_ "case" _ Expr _ Block)+
                      (_ "default" _ Block)?
                      _ "}"

Match              <- "match" __ Expr _ "{"
                      (_ "case" __ Expr _ Block)+
                      _ "}"

Receive            <- "receive" __ Expr _ "{"
                      (_ "case" _ Expr _ Block)+
                      (_ "timeout" _ DecimalIntegral _ Block)?
                      _ "}"

Tuple              <- "#(" _ ExprList? _ ")"

Array              <- "[" _ ExprList? _ "]" / "[" Expr _ ".." _ Expr "]"

Table              <- "[" _ (KeyValues / ":") _ "]"
KeyValues          <- KeyValue (_ "," _ KeyValues)*
KeyValue           <- Key _ ":" _ Value
Key                <- Expr
Value              <- Expr

Struct             <- "struct" _ Identifier

AnonFunctionDef    <- "fn" _ "(" _ Params _ ")" _ Block

FunctionCall       <- FunctionName _ "(" _ ExprList? _ ")"

ParanthesizedExpr  <- "(" _ Expr _ ")"

#
# Struct
#

StructDef          <- ("singleton" _)? "struct" _ Identifier _
                      ( ":" _ Interfaces _)? "{" _
                      StructMembers _ "}"
Interfaces         <- Identifier (_ "," _ Identifier)*
StructMembers      <- StructMember (_ "," _ StructMember)*
StructMember       <- Constructor / Deconstructor / MemberVariableDef /
                      MemberFunctionDef
Constructor        <- "this" _ "(" _ Params? _ ")" _ Block
Deconstructor      <- "~this" _ "(" _ Params? _ ")" _ Block
MemberVariableDef  <- MemberVisibility _ (MemberAccess _)? _ !"fn" Variable
                      (_ "=" _ Expr)?
MemberFunctionDef  <- MemberVisibility _ (MemberAccess _)? FunctionDef
MemberVisibility   <- "public" / "private"
MemberAccess       <- "const" / "readonly"

#
# Interface
#

InterfaceDef         <- "interface" _ Identifier _ "{" _ InterfaceMembers _ "}"
InterfaceMembers     <- InterfaceMember (_ "," _ InterfaceMember)*
InterfaceMember      <- InterfaceMemberVariableDef / InterfaceMemberFunctionDef
InterfaceMemberVariableDef
                     <- MemberVisibility _ (MemberAccess _)? _ !"fn" Variable
InterfaceMemberFunctionDef
                     <- MemberVisibility _ (MemberAccess _)?
                        InterfaceFunctionDef
InterfaceFunctionDef <- "fn" _ FunctionName _ "(" _ Params? _ ")"

#
# Enum
#

EnumDef            <- "enum" _ Identifier _ "{" _ EnumValues _ "}"
EnumValues         <- EnumValue (_ "," _ EnumValue)*
EnumValue          <- Identifier (_ "=" _ Expr)?

MainFunctionDef    <- "fn" _ "main" _ "(" _ Param? _ ")" _ Block
Param              <- Identifier
Block              <- "{" _ StatementSequence _ "}"
StatementSequence  <- Statement _ ";"  (_ StatementSequence)*
Statement          <- Assignment / MatchOperation / FunctionDef / Expr
Assignment         <- Variable _ "=" _ Expr
MatchOperation     <- Expr _ "<*" _ Expr
FunctionDef        <- "fn" _ FunctionName _ "(" _ Params? _ ")" _ Block
FunctionName       <- Identifier
Params             <- NonDefaultParams _ "," _ DefaultParams /
                      NonDefaultParams /
                      DefaultParams
NonDefaultParams   <- NonDefaultParam (_ "," _ NonDefaultParam)*
NonDefaultParam    <- Param !(_ "=")
DefaultParams      <- DefaultParam (_ "," _ DefaultParam)*
DefaultParam       <- Param _ "=" _ Expr

#
# Common stuff
#

ExprList           <- Expr (_ "," _ Expr)*
Variable           <- Identifier !(_ "(")
Identifier         <- [a-zA-Z_][a-zA-Z_0-9_]*
UnboundVariable    <- "?" Variable
#_                  <- WS*
#__                 <- WS+
_                   <- (WS / Comments)*
__                  <- (WS / Comments)+
WS                  <- [ \t\r\n]
Comments           <- SingleLineComment / BlockComment
SingleLineComment  <- "//" (!EOL .)* EOL?
EOL                <- "\r\n" / "\n" / "\r"
BlockComment       <- "/*" (!"*/" .)* "*/"
EOF                <- _ !.
```
