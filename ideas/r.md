# The R programming language

R is a dynamically typed language with type inference and

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
int, bignum, float, bool, tuple, fixed and dynamic array, hashtable, string,
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

# The main thing

Nothing can be declared in the global context except for the `main`
function which **must** be declared there.

```
fn main(_args) {
  "Hello World!"
}
```

The following words are reserved: `if`, `then`, `else`, `match`, `=`, `enum`, `:`,
`true`, `false`, `+`, `-`, `*`, `/`, `fn`, `[`, `]`, `{`, `}`,
`"`, `(`, `)`, `$`, `~`, `'`,  `<~`, `class`, `public`,
`private`, `readonly`, `const`, `this`, `new`, `interface`,
`singleton`, `.`, `import`, `?` and all other binary and unary
operators you can think of.  FIXME. Be cpo,eplete

## Comments

`//` and `/* ... */`

## Valid characters in symbols

`^[[:alpha:]_][[:alnum:]_]*$`

## Character literals

`'B' and '\n'` etc.

## Enum literals

```
enum Bonk {
  a
  b
  c
}
```

Used like this:

```
Bonk.a
Bonk.c
```

## Integral and floating-point literals

`3` is an int

`0xffff` is an int in hexadecimal format

`0b101010100` is an int in binary format

`017` is an int in octal format

`3.0B` is a bignum

`3.0` is a float

`1.23e6` is a scientific float

No automatic casting is performed between integral, bignum and
floating-point type and casting must be done explicitly. `#`is the
cast operator and it only knows about the `int`, `float` and `big`
types. If a value already is of the required type the casting becomes
a noop.

```
a = 3,
b = 042
c = 39879879284379287
d = 3.0
e = a + int#d * b                // 105
f = c / big#(d + float#a)
```





!b + big!d



c = big!a + b + int!;


c = + b + int!;



d = a + (float!b - 1.0)
```


## Boolean literals

```
true
false
```

## String literals

Immutable

`"foo"`

`"foo $a is not ${a + 1.0}"` becomes `"foo 3.0 is not 4.0"`

`r"foo\nbar"` is a raw string without escape processing

```
a = "foo"
b = "bar"
c = a ~ b               // c = "foobar" (COPY!)
```

## Variables

```
aVariable = 1
B52 = "foo"
```

## Tuples

```
'(1, 2)
'(a, '(b, 4))
```

## Dynamic arrays

All elements in an array must have the same type:

```
[int!4711.0, 42]        // A valid array literal
[4711, 42]              // A valid array literal
a = [1, 2, 3, 4, 5]
b = a[1 .. 3]           // b = [2, 3]
c = a[2 .. $ - 1]       // c = [3, 4]
d = b ~ c               // d = [2, 3, 3, 4] (COPY!)
d[1] = 42               // d = [2, 42, 3, 4]
a[2] = 23               // a = [1, 2, 23, 4, 5]
                        // b = [2, 23]
                        // c = [23, 4]
                        // d = [2, 42, 3, 4]
e = b.dup()             // Explicit copy
b = [4711] ~ b          // b = [4711, 2, 23] (COPY!)
e ~= 4711               // e = [2, 23, 4711]
f = a[$ / 2 .. $]       // What do we get?
g = a
h = a.dup()
g is a                  // true
h is a                  // false
g == a                  // true
h == a                  // true
```

Above we only examplify with integers but all available types in R can
be stored in dynamic arrays.

> [!NOTE]
> Internally implemented with a double-ended queue (dynamic
> array of continous memory).

## Hashtables

All keys and values may have any type:

```
[ "no" : 1.0 ]          // A hashtable literal
a = [ "a" : 1.0, "b" : "foo" ]
a["a"] = "bar"
a[42] = "baz"           // a = ["a" : "bar", "b" : "foo", 42 : "baz"]
b = a                   // b = ["a" : "bar", "b" : 0, 42 : "baz"]
b["a"] = 0              // a = ["a" : 0, "b" : 0, 42 : "baz"]
                        // b = ["a" : 0, "b" : 0, 42 : "baz"]
c = a["a"]              // c = 0
d = a ~ [42 : 4711]     // d = [42 : 4711, "a" : 0, "b" : 0] (COPY!)
e = b.dup()             // Explicit copy
```

> [!NOTE]
> Structural equality is used for all key values (even for class
> instances)

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

  public fn foo() {
    0
  }

  private fn bar(b) {
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

if Foo is availble in module `bar` (read more aboy hierarchical modules below). Access to member variables and functions look like this:

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
class Foo <@ Bar Bonk {
   // See Foo class above
}
```

Above `Foo` implements two interfaces `Bar` and `Bonk`.

A class can also be defined as a singleton:

```
singleton class Foo <@ Bar {
   // See Foo record above
}
```

It means what you think.

If you need to define a bunch of constants you typically do this in a
singleton class like this:

singleton class Math {
    const PI = 3.1
    const SQUARE2 = math:sqrt(2)
}

## Control flow

```
if expr {
  a
} else {
  b
  c
}
```

switch expr {
case "foo:
  1
default:
  ss
}

## Functions

Define a named function like this:

```
fn foo(a, b, c = 0) {
  c
  d
}

fn foo(a = 1) {
  a
}
```

Note how trailing parameters may have default values.

> [!NOTE]
> No support for currying and variadic parameters

Calling convention:

`foo(2, 6)`

or

`bar:foo(2, 6)`

if `foo` is implemented in module `bar`.

Functions may be nested:

```
fn foo(a, b, c = 0) {
  fn bar(d) {
    d
  }
  bar(a)
}
```

Define an anonymous function like this:

```
fn (a, b) {
    b
}
```

This is an example of a map function:

```
fn main() {
    l = [1, 2, 3]
    f = fn (l, n) { l[n] + 1 }
    true <~ map(l, f)
}

fn map(l, f, n = 0) {
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

```
a = 1
fn foo(ref b) {
    b += 1
}
foo(a)
writeln(a)                  // 2
```

## Matching

Matching can be done with the `<~` operator:

```
a = 1
'(a, ?a, 1) <~ '(1, 2, 1)   ; a = 2
'(?a, b, ?h) <~ foo(42)
[1, ?a] <~ [1, 2]           ; a = 2
[42 : 1, "foo" : ?a] <~ [42 : 1, "foo" : 2]    ; a = 2
```

`?` introduces an unbound variable.

Matching can also be done like this:

```
match expr {
  case match-expr {
    a
    b
  }
  case match-expr {
    c
  }
}
```

As seen here:

```
a = 1
b = 3
match expr {
  case '(1, ?a) {
    a
  }
  case a || b {
    a + 1
  }
  case _ {
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

> [NOTE!]
> All jobs get their own copy of all singelton classes. Nothing must
> be shared.

`spawn` returns a job id (jid) which can be used to send messages to
the job with the `send` keyword:

`send jid '(timeout, 1000)`

A message sent to a job ends up in its mailbox and can be retrieved
with the `receive` keyword:

```
receive {
    case '(?jid, ?result) {
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
until the mailbox has less messages in its mailbox.

As an alternative to `OnCrowding.block` `OnCrowding.ignore` can be
used to specify that overflowing messages should be ignored. The
`OnCrowding` enum can alternatively be replaced with a function that
returns `false` if overflowing messages should be ignored or `true` if
`send` should continue to block.

The last concurrency keyword is `self`. It is a sibling to `this` in
classes, but it return tge `jid` for the currently running job.

The `std.concurrency` module also contains these functions:

`monitor(jid)`: Send a message `'(JobMonitor.died, jid)` to me if this
job dies.

`link(jid)`: Send a message `'(JobMonitor.died, jid)` to me if this
job dies. Do the same to the linked job if I die.


`kill(jid)`: Does what you think.

singleton Ackermann

A small example may clear things up. Below is a main function which
spawns jobs to compute Ackermann function values for the parameters m
= 3, n = 1 .. 10. The `main` function uses the Ackermann singleton
class to start 10 jobs and then waits for all jobs to return a result.

```
main() {
  jids = Ackermann.startJobs(3, 10)
  waitForJobs(jids)
}

singleton class Ackermann {
    public startJobs(m, n, jids = []) {
        if jids.length <= n {
            computeAckermann(fromJid, m, n) {
                result = ackermann(m, n)
                send fromJid '(self, result)
            }
            jid = spawn computeAckermann(self, m, n)
            setMaxMailboxSize(jid, 4, OnCrowding.block)
            startJobs(m, n + 1, jids ~ jid)
        }
        jids
    }

    public waitForJobs(jids) {
        if jids.length > 0 {
            receive {
                case '{?jid, ?result} {
                    writeln("Compute job $jid sent us the result $result)
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
