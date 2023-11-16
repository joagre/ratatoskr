# The Satie Programming Language

## Introduction

Satie is envisioned as a forward-thinking programming language, ideal
for crafting the programming editor of tomorrow. Its capabilities
extend beyond merely constructing the editor; it is also adept at
serving as the scripting language for creating editor plugins and
customizations. Yet, the essence of Satie lies in its versatility — it
is a purpose-built language and Virtual Machine (VM) that boasts a
high degree of generality, adaptable to a wide range of applications.

Satie owes much to the great people behind the
[Erlang](https://www.erlang.org/) and [D](https://dlang.org/)
programming languages (and all people standing behind [and beside] them).

All rise.

```
$ cat hello.sa
import std.stdio : writeln
import std.lists

export fn main(args) {
    ?n = args[1],
    ?jobs = hello(n),
    lists.foreach(fn (job) {
        job <| "Standing on the shoulders of giants"
    }, jobs)
}

fn hello(n, jobs = []) {
    if n > 0 {
        ?job = spawn fn () {
            receive {
                case ?message {
                   writeln("$n: $message")
                }
            }
        },
        hello(n - 1, job ~ jobs)
    } else {
        jobs
    }
}
$ sac hello.sa && sa hello 100000
0: Standing on the shoulders of giants
1: Standing on the shoulders of giants
2: Standing on the shoulders of giants
3: Standing on the shoulders of giants
...
...
99999: Standing on the shoulders of giants
100000: Standing on the shoulders of giants
```
*Source: [hello.sa](../grammar/hello.sa)*

That said.

The following design choices have been made (in some sort of order):

 * Satie is built on a custom built multi-core VM with strong support
   for  time sliced green threads (from now on called *jobs*). Jobs
   have share nothing semantics relying solely on message passing to
   make it easier to reason about, and implement highly concurrent,
   massively scalable soft real-time systems with an emphasis on fault
   tolerance and high availability. Jobs can create monitors and links
   between each other and this makes it possible to write supervisor
   jobs that are responsible to restart jobs if they should die
   unexpectedly.

 * Satie is a pure functional programming language with native
   persistent datatypes in its core. All data is immutable and the
   persistent datatypes have been custom built to efficiently handle
   large amount of data. There are limits to Satie's purity though. No
   monads.

 * Satie tries to be a balanced blend between a fully fledged
   application language and a script language for reasons given
   above. This shines through in its choice of semantics, syntax, type
   system, object-orientation support and more.

 * Satie is dynamically typed and comes with a small set of basic
   types (`bool`, `int`, `flot`, `char`, `function`, `job` and `enum`)
   and composite types (`string`, `tuple`, `list`, `map` and
   `class`). The compiler uses Hindley–Milner type inference to deduce
   the types of variables, expressions and functions. Semantic and
   syntactic care has been taken to make it feasible to add a gradual
   type system later on.

 * Satie's dynamic type system relies on a Garbage Collect (GC)
   mechanism that takes great care to do garbage collection on a job
   basis. All to avoid the GC mechanism becoming a stop-the-world
   activity.

 * Satie is a small language and should be easy to learn. It has a
   clean, regular and minimalist syntax adhering to the school of
   curly braces languages, i.e. it uses well known reserved words,
   syntax and scoping rules. The element of least surprise has been a
   leading principle but in Satie everything is an expression, i.e. no
   statements to be seen (and no semicolons). Because of this certain
   syntactical constructs may seem unorthodox even though they
   represent a regular and consistent syntax. The syntax of the D and
   Erlang programming language have been heavy influencers when
   applicable and another leading principle has been to make the
   syntax familiar and easy on the eye, but you have to be the judge
   on that. Satie reserves 24 keywords and sports 16 operators and the
   complete syntax is formally defined as a PEG grammar in appendix B.

 * Satie has pattern matching in its core and the `=` operator is
   actually all about pattern matching rather than assignment (*there
   must not be mutable updates*). Everything can be matched and taken
   apart with the help of a pattern matching in combination with the
   `=` operator. Pattern matching is also used by the `match`
   expression which is a sibling to `switch` but on pattern matching
   speed. The `receive` expression also uses pattern matching to do
   selective receive on messages in a job's mailbox.

 * Satie is implemented using a custom built VM consisting of a
   multi-core and time slicing job scheduler running multiple
   instances (one for each job) of a custom built register
   machine. The VM and its scheduler has a small memory footprint and
   each job startedinitially only allocates 1KB for its heap and stack
   (program code excluded). The VM is standalone and has few
   dependencies making it easy to port to restricted targets.

 * Great care has been taken to add a purely functional and
   encapsulating `class` definition. It makes it possible group member
   variables and member functions using well known C++/Java member
   modifiers such as `public`,  `private`, `const` and `this`
   references and more.

Many things are by design not a part of Satie:

 * Pointers
 * Type specifications
 * Exceptions
 * Pre-processor and macros
 * Variadic function parameters
 * Mutexes (not needed)
 * Mutability
 * Currying
 * Inheritance
 * Monads

and more I am sure you will miss.

## Overall structure

A Satie file has a `.sa` suffix and constitute a Satie *module* with
the same name as the basename of its filename. Why make it more
complicated? A satie file starts with a number of import statements
which are followed by a mix of `enum`, `interface`, `class` and `fn`
(function) definitions (in any order).

`class`, `enum`  and `interface` definitions are **only** allowed on
the top level of a module. Function definitions can be nested
arbitrarily within other function definitions though.

A single exported main function must be defined in exactly one of the
Satie modules that constitutes an application.

Example:

```
import std.stdio : writeln

enum Color {
    red
    green
    blue
}

interface Iterator {
    public fn next()
    public fn hasNext()
}

class ColorIterator : Iterator {
    private colors
    private graffiti

    this(colors) {
        this(colors: colors, graffiti: "Killroy was here")
    }

    public fn next() {
        if (!hasNext()) {
            false
        } else {
            #(this(colors: colors.rest()), colors.first())
        }
    }

    public fn hasNext() {
        !colors.isEmpty()
    }
}

export fn main() {
    ?colors = [Color.red, Color.red, Color.blue, Color.green],
    ?iterator = new ColorIterator(colors),
    fn iterate(iterator) {
        if (iterator.hasNext()) {
            #(?nextIterator, ?color) = iterator.next(),
            writeln("Color: $color"),
            iterate(nextIterator)
        }
    },
    iterate(iterator)
}
```
*Source: [color.sa](../grammar/color.sa)*

That was very boring but hopefully informative. Noteworthy is that
the Satie module above has one exported function (the famous `main`
function). A module can define as many functions it needs on the top
level (exported or not) but it is a good idea to define functions as
member functions in classes to avoid function cluttering. Only
functions being marked with `export` can be imported by other
modules. The `class`, `enum` and `interface` definitions can be
imported by other modules without restrictions though.

`#(` .. `)` is a tuple and the question mark before a variable informs
the compiler that it is to be seen as unbound (even if it was bound
before). If the question mark is omitted the compiler makes sure
that the variable is already bound, and a run-time a check is made to
verify that the bound variable matches the rvalue. This may sound
harsh but match patterns are also used by the `match` and `receive`
expressions (see below).

The final thing that might be confusing is how the `this` constructor
calls itself, but this is the final step a constructor has to perform
to actually initialize its member variables. The `:` notation is
Satie's way to call a function with named parameters and a constructor
can also call itself with its member variables as named parameters.

That is it. The rest is in the gory details.

# Building and executing

Satie's compiler is called `sac` and the byte code it produces can be
executed with the `sa` runner. This was done in the introduction chapter
above and is repeated here for clarity:

```
$ ls
hello.sa
$ sac hello.sa && sa build/hello 100000
0: Standing on the shoulders of giants
1: Standing on the shoulders of giants
2: Standing on the shoulders of giants
3: Standing on the shoulders of giants
...
99999: Standing on the shoulders of giants
100000: Standing on the shoulders of giants
$ find .
.
./hello.sa
./build
./build/hello.sab
```

That is it.

If a Satie application consists of many modules in a directory
hierarchy the process above is the same. Say for example that you have
an application called `zarah` with the following directory hierachy:

```
$ cd zarah
$ find .
.
./src
./src/main.sa
./src/utils
./src/utils/database.sa
./src/utils/httpclient.sa
```

This is how this application can be compiled and executed:

```
$ sac src/main.sa
$ find .
.
./build
./build/main.sab
./build/utils
./build/utils/httpclient.sab
./build/utils/database.sab
./src
./src/main.sa
./src/utils
./src/utils/database.sa
./src/utils/httpclient.sa
$ sac build/main
```

> [!NOTE]
> The `sac` compiler has options to point to alternative `src/` and
> `build/` directories.

The compiler by default follows module dependencies introduced by
`main.sa` and automatically compiles these modules as well. The
compiler can be made not to follow module dependecies, ignore modules
that are missing or not possible to compile (for some reason) etc.

Read more about the `sac` compiler and the `sa` runner in their
respective manual page.

# The shell

The `sa` runner can also be made to start an interactive Satie shell:

```
$ sa --shell
Satie <0.3.1> (press ctrl-c to abort)
0> _
```

In the shell Satie expressions can be evaulated and the status of an
already executing application can be inspected, i.e a shell can be
made to connect to an already executing instance of a `sa`
runner. Read more about this and more in the `sa` runner's manual page.

# Comments

HERE

# Types





Pattern matching
Compile run repl

# pattern matching



# Comments

Everything after `//` and to end of line and within `/* ... */` are
considered comments.

# Types

## Basic types

`bool` : Boolean value `true` or `false`

`int` : Signed arbitrary-precision integer. On a 64-bits target
machine integers are handled natievly if they fit within 61 bits. If
they don't they are transparently represented as bignums. On a 32-bits
machine they must fit within 29 bits.

`float` : Floating point with a precision decided by the target
machine minus three bits.

`char` : 32-bits Unicode code point

`function` : Function reference

`job` : Job reference

`enum`: Enumeration reference

FIXME: Add a chapter about Enums

## Composite types

`string` : Immutable sequence of UTF-8 encoded characters

`tuple` : A fized sized sequence of values of any type

`list`: A list of values of any type

`map`: A mapping between a key of any type and a value of any type

`class` : Unit of encapsulation for member variables and functions

## Type management

All values can be type checked in run-time using the functions
`isBool`, `isInt`, `isFloat`, `isChar`, `isFunction`, `isJob`,
`isEnum`, `isString`, `isList`, `isMap`, `isObject` and `typeof`.

Example:

```
?a = 3.14,
a.isInt(),      // false
a.isFloat(),    // true
a.typeof()      // "float"
```

Operators which operate on `int` and `float` values require the
operands to be of the same type. No implicit numeric conversion is
performed. A `cast` operator exists to cast between `int` and `float`
values.

Example:

```
3,
93326215443944152681,
3.0,
b / cast(int)c + a,      // 31108738481314713603
d + c                    // Compiler error!
```

The `inspect` function provides even more run-time type information.

Example:

```
enum Foo {          //  Defined in bar.sa
  a = 3.14
  b
}

?c = Foo.a,
?d = [1, "foo"],
c.inspect(),        // [ "type" : "enum",
                    //   "name": "a",
                    //   "value": 3.14,
                    //   "owner" : #("bar", "Foo") ]
d.inspect()         // [ "type" : "list", "length": 2 ]
```

> [!NOTE]
> The enumeration value in the example above would normally be
> accessed with `c.value` and the list length with `d.length`

All values can be convrted to string representation using the
`toString` function.

Example:

```
?a = 3.14,
?b = [Foo.a : 42, "bar": #(fn () { x + x}, [1, 2,3])],
a.toString(),       // "3.14"
b.toString()        // "[Foo.a : 42, "bar": fn/0]"
```

# Identifiers

Identifiers are case sensitive strings of characters starting with an
ASCII letter or an underscore, followed by any number of ASCII
letters, underscores or digits,
i.e. `^[[:alpha:]_][[:alnum:]_]*$`. Keywords, variables, function
names, class names and enum names are all identifiers.

> [!NOTE]
> By design only strings can contain Unicode characters. This
> restriction may be lifted if compelling reasons should appear.

## Keywords

The following 26 special identifiers are language-reserved and cannot
be defined in user code:

```
import
true
false
enum
in
is
fn
export
if
else
switch
default
match
class
interface
public
private
readonly
const
this
spawn
monitor
link
receive
timeout
self
```

# Literals

## Boolean literals

`true` or `false`

## Integral literals

Integral literals can be formatted as decimal, octal and hexadecimal
values.

Examples:

```
4,             // Decimal format
017,           // Octal format
0xffff,        // Hexadecimal format
0b101010100    // Binary format
```

## Floating-point literals

Examples:

```
1.0,
.666e2
```

## Character literals

A character literal is a Unicode code point enclosed within single
quotation marks. It consists of four bytes.
Examples:

```
'A',
'ω',
'\u03c9'    // ω
```

## Function literals

Function literals follow the same syntax as regular function
definitions (see below) but with a function name.

Example:

```
?sum = fn (x, y) { x + y },
?a = sum(1, 2)                // a = 3
```

## Job literals

Job literas are opaque.

## Enum literals

Example:

```
enum Color {
    red,
    green,
    blue
}
```

The `Color` enumeration above introduces the literals `Foo.red`,
`Foo.green` and `Foo.blue`.

## String literals

String literals are represented as comma separated sequence UTF-8
encodedsequences of Unicode characters enclosed within double
quotation marks. Escape sequences has meaning in double quoted
strings. Raw strings are also enclosed within double quotation marks
but are prefixed with the letter `r`. Escape sequences have no meaning
in raw strings and all characters are parsed verbatim.

Examples:

```
?a = "fooω",
a[3],               // 'ω'
?b = r"foo\nbar"    // b.length == 8
```

## Tuple literals

Tuple literals are represented as comma separated fixed size sequences
of values of any type enclosed between a leading `#(` and a trailing
`)`.

Example:

`#("foo", 3.14, #("bar", fn (x) { x + 1}))`

## List literals

List literals are represented as comma-separated sequences of values of
any type enclosed within square brackets.

Example:

```
?a = [3.14, "foo", 1816381],
?b = a[1 = 42, 2 = "bar"]      // b = [3.14, 42, "bar"]
```

> [!NOTE]
> Only existing list entries can be updated this way

## Map literals

Map literals are represented as comma-separated sequence of key-values
of any type (separated by a `:` character) enclosed within square
brackets.

Example:

```
?a = ["foo" : 12, 3.14 : 981237198192378 ],
a[3.14: 4711, 2 : 4]                         // ["foo" : 12, 3.14: 4711, 2 : 4],
```

## Class literals

Class literals are represented as semicolon-separated sequences of
member-values, where the member is a class member name and value is of
any type, (separated by a `=` character) enclosed in square brackets.

Example:

```
class Foo P {
    public foo = 4711
    public bar = "foo"
    public zonk = #(1, 3.14)
}


?a = new Foo(),
[foo ; ?b, bar ; ?b] = a,
b,                          // 4711
c                           // "foo"
```

# Top level contructs and expressions

Everything is an expression in Satie except for the top level
definitions, i.e. `import`, `class`, `interface`, `enum` and named `fn
defintions.

Binding of names can only be performed as a standalone expression and
cannot no be done deep within an expressions. For your own sake,

Example:

main() {
    ?a = 42,
    ?b = a + (?c = 42 + a) + a    // Compiler error!
}

# Control flow

## `{` a, b, c, ... `}' expression

A block expression is a comma-separated sequence of expressions
enclosed in curly braces. Expressions are evaluated in a sequence and
introduces a lexical scope. An identifier bound in a scope is visible
to all the following expressions in the scope. The identifier is not
visible outside of the scope and it shadows an identifier with the
same name introduced outside of the scope. The value of the last
expression in the sequence is returned from the block.

Example:

```
main() {                  // A function block starts here
    ?a = 42,
    ?d = {                // Local block starts here
        ?b = a + 1,
        ?a = b
     }
    // b is not defined
    // a = 42
    // d = 43
}
```

## `if`, `else` expression

Example:

```
?a == 4,
?b = if a == 4 {
         42;
     } else {
         c,
         d
     },
a         // 42
```

## `switch`, `case`, `default` expression

Example:

```
?a = "foo",
?b = switch a {
    case "foo" {
        42;
    }
    case foo(c) {
        a + 1;
    }
    default {
        a;
    }
},
b          // 42
```

There is no fall through mechanism and the `default` keyword is optional.

## Tuples

Example:

```
?a = 42,
?b = #(4711, #(a, [1, 2])),
#(_, #(?a, [_, c])) = b;
a,                           // 4711
c                            // 2
```

## Arrays

Lists containin elements of any type. Arrays support slicing which
makes it easy to work with portions of list.

HERE first rest


`a[i .. j]` returns a list slice which starts at index `i` and ends
with index `j - 1`. No data is copied from the origin array, i.e. if the
slice is updated the origin array will also be be updated. `i` and `j`
can any valid expressions and the keyword `$` is the length of the
array.


`arr[i]` access the i:th element of an array or map. For an array
`i` must be an integral type and for a map it can be a key of any
type. If the indexing expression is an `lvalue` in an assignment the
expression inserts a value in the array or map.

Example:

`a[i] = 0;`


Examples:

```
a = []                 // An empty array
a = [1, 2, 3, 4, 5]
a.first()              // 1
a.rest()               // [2, 3, 4, 5]
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

## Function calls

`fun(a, b)` invokes the function `fun` with a comma separated argument
list of expressions. Arguments are evaluated left to right before the
function is invoked. `fun` can refer to the name of a defined function
or a function literal.


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





import a.b.c
  c.foo()
  c.Type

import d = a.b.c
  d.foo()
  d.Type

import a.b.c : Type;
  c.foo()
  Type











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

# `class` defintion

Structs encapsulate member values and member functions and they can
only be defined on the top-level defintions of each module.

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

`job <- `#(timeout, 1000)`

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
in `<-` waiting for the mailbox to shrink.

`OnCrowding.ignore` can be used instead `OnCrowding.block` to specify
that overflowing messages should be ignored. The `OnCrowding` enum can
alternatively be replaced with a function that returns `false` if
overflowing messages should be ignored or `true` if the sending job
should be blocked in `<-`.

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
import std.jobs : OnCrowding, Job
import std.stdio
import std.lists

export fn main() {
  ?ackermann = new Ackermann(),
  ?ackermann = ackermann.startJobs(3, 10),
  ackermann.waitForJobs()
}

class Ackermann {
    private jobs = []

    public fn startJobs(m, n, i = 0, startedJobs = []) {
        if i < n {
            fn computeAckermann(parentJob, m, n) {
                ?result = ackermann(m, n),
                parentJob <| #(self, m, n, result)
            },
            ?job = spawn monitor computeAckermann(self, m, i),
            job.setMaxMailboxSize(job, 4, OnCrowding.block),
            startJobs(m, n, i + 1, job ~ startedJobs)
        } else {
            this(jobs: startedJobs)
        }
    }

    public fn waitForJobs() {
        fn waitForJobs(jobs) {
            if jobs.length > 0 {
                receive {
                    case #(?job, ?m, ?n, ?result) {
                        stdio.writeln("ackermann($m, $n) = $result"),
                        waitForJobs(jobs.delete(job))
                    }
                    case #(Job.died, ?job, ?reason) {
                        stdio.writeln("Oh no! Compute job $job died: $reason")
                    }
                }
            } else {
                this(jobs: [])
            }
        },
        waitForJobs(jobs)
    }

    private fn ackermann(m, n) {
        if m == 0 {
            n + 1
        } elif n == 0 {
            ackermann(m - 1, 1)
        } else {
            ackermann(m - 1, ackermann(m, n - 1))
        }
    }
}
```

# Appendix A: Operator precedence

Operators in decreasing order of precedence:

| Expression   | Description                              |
|--------------|------------------------------------------|
| a.b          | Field access                             |
| a(b, c)      | Function call                            |
| a[i]         | Indexing                                 |
| a[b .. c]    | List slicing                             |
| -a           |                                          |
| +a           |                                          |
| !a           |                                          |
| ~a           | Bitwise complement                       |
| <-           | Send message                             |
| cast(t)a     | Cast expression                          |
| a ^^ b       | Exponentiation                           |
| a * b        |                                          |
| a / b        |                                          |
| a % b        | Modulus                                  |
| a + b        |                                          |
| a - b        |                                          |
| a ~ b        | Concatenation                            |
| a &lt;&lt; b |                                          |
| a >> b       |                                          |
| a >>> b      | Unsigned right shift                     |
| a in b       | Map membership                           |
| a == b       | Equality test (a == b == c is not legal) |
| a != b       |                                          |
| a is b       | Identity test                            |
| a !is b      | !(a is b)                                |
| a &lt; b     |                                          |
| a &lt;= b    |                                          |
| a > b        |                                          |
| a >= b       |                                          |
| a \| b       |                                          |
| a ^ b        | Bitwise xor                              |
| a & b        |                                          |
| a && b       | Logical and                              |
| a \|\| b     |                                          |
| a isnow b       | Transform                                |
| a = b        |                                          |

# Appendix B: PEG grammar

```
#
# Top level structure
#

Program <- _ (Imports __)? TopLevelDefs EOF
TopLevelDefs <- TopLevelDef (__ TopLevelDef)*
TopLevelDef <- ClassDef / InterfaceDef / EnumDef / FunctionDef

Imports <- Import (__ Import)*
Import <- "import" __ (ModuleAlias _ "=" _)? _ ModulePath
ModuleAlias <- Identifier
ModulePath <- Identifier ("." Identifier)* (_ ":" _ ImportedEntities)?
ImportedEntities <- Identifier (_ "," _ Identifier)*

#
# Expression
#

Expr <- BindExpr
BindExpr <- (Literal / UnboundVariable / Identifier) (_ "=" _ Expr) / SendExpr
SendExpr <- ("self" /
             ControlFlowExpr /
             SpawnExpr /
             Identifier /
             "(" _ Expr _ ")") (_ "<|" _ Expr) / LogicalOrExpr
LogicalOrExpr <- LogicalAndExpr (_ "||" _ LogicalAndExpr)*
LogicalAndExpr <- BitwiseAndExpr (_ "&&" _ BitwiseAndExpr)*
BitwiseAndExpr <- BitwiseXorExpr (_ "&" _ BitwiseXorExpr)*
BitwiseXorExpr <- BitwiseOrExpr (_ "^" _ BitwiseOrExpr)*
BitwiseOrExpr <- LargerThanEqualExpr (_ "|" _ LargerThanEqualExpr)*
LargerThanEqualExpr <- LargerThanExpr (_ ">=" _ LargerThanExpr)*
LargerThanExpr <- LessThanEqualExpr (_ ">" _ LessThanEqualExpr)*
LessThanEqualExpr <- LessThanExpr (_ "<=" _ LessThanExpr)*
LessThanExpr <- IsNotExpr (_ "<" _ IsNotExpr)*
IsNotExpr <- IsExpr (_ "!is" _ IsExpr)*
IsExpr <- NotEqualExpr (_ "is" _ NotEqualExpr)*
NotEqualExpr <- EqualExpr (_ "!=" _ EqualExpr)*
EqualExpr <- InExpr (_ "==" _ InExpr)*
InExpr <- UnsignedRightShiftExpr (_ "in" _ UnsignedRightShiftExpr)*
UnsignedRightShiftExpr  <- RightShiftExpr (_ ">>>" _ RightShiftExpr)*
RightShiftExpr <- LeftShiftExpr (_ ">>" _ LeftShiftExpr)*
LeftShiftExpr <- ConcatenateExpr (_ "<<" _ ConcatenateExpr)*
ConcatenateExpr <- MinusExpr (_ "~" _ MinusExpr)*
MinusExpr <- PlusExpr (_ "-" _ PlusExpr)*
PlusExpr <- ModulusExpr (_ "+" _ ModulusExpr)*
ModulusExpr <- DivideExpr (_ "%" _ DivideExpr)*
DivideExpr <- MultiplicateExpr (_ "/" _ MultiplicateExpr)*
MultiplicateExpr <- ExponentiationExpr (_ "*" _ ExponentiationExpr)*
ExponentiationExpr <- CastExpr (_ "^^" _ CastExpr)*
CastExpr <- "cast" _ "(" _ ("int" / "float") _ ")" _ SendMessageExpr /
            SendMessageExpr
SendMessageExpr <- BitwiseComplementExpr (_ "*" _ BitwiseComplementExpr)*
BitwiseComplementExpr <- "~" _ NotExpr / NotExpr
NotExpr <- "!" _ UnaryPlusExpr / UnaryPlusExpr
UnaryPlusExpr <- "+" _ UnaryMinusExpr / UnaryMinusExpr
UnaryMinusExpr <- "-" _ PostfixExpr / PostfixExpr
PostfixExpr <- PrimaryExpr _ ("." _ (ControlFlowExpr / Identifier) /
                              "(" _ Args? _ ")" /
                              "[" _ Expr _ "]")*

PrimaryExpr <- "this" /
               "self" /
               "$" /
               Literal /
               ControlFlowExpr /
               SpawnExpr /
               NewExpr /
               UnboundVariable /
               Identifier /
               "(" _ Expr _ ")"

Literal <- BooleanLiteral /
           NumberLiteral /
           CharacterLiteral /
           StringLiteral /
           FunctionLiteral /
           TupleLiteral /
           (Identifier _)? ListLiteral /
           (Identifier _)? MapLiteral /
           ClassLiteral

BooleanLiteral <- "true" / "false"

NumberLiteral <- FloatingPoint / Integral
Integral <- HexIntegral / BinaryIntegral / OctalIntegral / DecimalIntegral
FloatingPoint <- DecimalPointLeading / DecimalPointTrailing
DecimalPointLeading <- [0-9]* "." [0-9]+ ExponentPart?
DecimalPointTrailing <- [0-9]+ ExponentPart
ExponentPart <- [eE] [+-]? [0-9]+
DecimalIntegral <- [0-9]+
OctalIntegral <- "0" [0-7]+
HexIntegral <- "0x" [0-9a-fA-F]+
BinaryIntegral <- "0b" [01]+

CharacterLiteral <- "'" ( Escape / NonQuoteChar ) "'"
Escape <- "\\" ( [abfnrtv'"\\] /
                 "x" HexDigit HexDigit /
                 "u" HexDigit HexDigit HexDigit HexDigit /
                 "U" HexDigit HexDigit HexDigit HexDigit
                     HexDigit HexDigit HexDigit HexDigit /
                  OctalDigit /
                  OctalDigit OctalDigit /
                  OctalDigit OctalDigit OctalDigit )
HexDigit <- [0-9a-fA-F]
OctalDigit <- [0-7]
NonQuoteChar <- [^']

StringLiteral <- RegularString / RawString
RegularString <- '"' ( EscapeSequence / [^"] )* '"'
EscapeSequence <- "\\" [btnvfr"\\]
RawString <- 'r"' [^"]* '"'

FunctionLiteral <- "fn" _ "(" _ Params? _ ")" _ BlockExpr

TupleLiteral <- "#(" _ Exprs? _ ")"
Exprs <- Expr (_ "," _ Expr)*

ListLiteral <- "[" _ Exprs? _ "]" /
               "[" Expr _ ".." _ Expr "]" /
               "[" _ IndexValues _ "]"
IndexValues <- IndexValue (_ "," _ IndexValue)*
IndexValue <- DecimalIntegral _ ":" _ Expr

MapLiteral <- "[:]" / "[" _ KeyValues? _ "]"
KeyValues <- KeyValue (_ "," _ KeyValue)*
KeyValue <- (Literal / Identifier) _ ":" _ Expr

ClassLiteral <- "[" _ MemberValues? _ "]"
MemberValues <- MemberValue (_ "," _ MemberValue)*
MemberValue <- Identifier _ ";" _ Expr

ControlFlowExpr <- IfExpr / SwitchExpr / MatchExpr / ReceiveExpr / BlockExpr

IfExpr <- "if" __ Expr _ BlockExpr
          (_ "elif" __ Expr _ BlockExpr)*
          (_ "else" _ BlockExpr)?

SwitchExpr <- "switch" __ Expr _ "{"
              (_ "case" _ Expr _ BlockExpr)+
              (_ "default" _ BlockExpr)? _ "}"

MatchExpr <- "match" __ Expr _ "{"
             (_ "case" __ Expr _ BlockExpr)+ _ "}"

ReceiveExpr <- "receive" _ "{"
               (_ "case" __ Expr _ BlockExpr)+
               (_ "timeout" _ DecimalIntegral _ BlockExpr)? _ "}"

SpawnExpr <- "spawn" (__ "monitor" / "link")? __ Expr

NewExpr <- "new" _ Identifier _ "(" _ Args? _ ")"

UnboundVariable <- "?" _ Identifier

Identifier <- [a-zA-Z_][a-zA-Z_0-9_]*

#
# Class definition
#

ClassDef <- "class" __ Identifier _ ( ":" _ Interfaces _)?
                   "{" _ ClassMembers _ "}"
Interfaces <- Identifier (_ "," _ Identifier)*
ClassMembers <- ClassMember (_ ClassMember)*
ClassMember <- Constructor / Deconstructor / MemberFunction / MemberVariable
Constructor <- "this" _ "(" _ Params? _ ")" _ BlockExpr
Deconstructor <- "~this" _ "(" _ Params? _ ")" _ BlockExpr
MemberFunction <- MemberAccess _ FunctionDef
MemberAccess <- "public" / "private"
MemberVariable <- (MemberAccess (_ "const")? / "readonly") _ Identifier
                  (_ "=" _ Expr)?

#
# Interface definition
#

InterfaceDef <- "interface" __ Identifier _ "{" _ InterfaceMembers _ "}"
InterfaceMembers <- InterfaceMember (_ InterfaceMember)*
InterfaceMember <- InterfaceMemberFunction / InterfaceMemberVariable
InterfaceMemberFunction <- MemberAccess _ InterfaceFunction
InterfaceFunction <- "fn" _ Identifier _ "(" _ Params? _ ")"
InterfaceMemberVariable <- (MemberAccess (_ "const")? / "readonly") _ Identifier

#
# Enumeration definition
#

EnumDef <- "enum" __ Identifier _ "{" _ EnumValues _ "}"
EnumValues <- EnumValue (__ EnumValue)*
EnumValue <- Identifier (_ "=" _ Expr)?

#
# Function definition
#

FunctionDef <- ("export" _)?
               "fn" __ Identifier _ "(" _ Params? _ ")" _ BlockExpr
Params <- NonDefaultParams _ "," _ DefaultParams /
          NonDefaultParams /
          DefaultParams
NonDefaultParams <- NonDefaultParam (_ "," _ NonDefaultParam)*
NonDefaultParam <- Identifier !(_ "=")
DefaultParams <- DefaultParam (_ "," _ DefaultParam)*
DefaultParam <- Identifier _ "=" _ Expr

BlockExpr <- "{" _ BlockLevelExprs _ "}"
BlockLevelExprs <- BlockLevelExpr (_ "," _ BlockLevelExpr)*
BlockLevelExpr <- FunctionDef / Expr

Args <- PositionalArgs / NamedArgs
PositionalArgs <- !NamedArg Expr (_ "," _ Expr)*
NamedArgs <- NamedArg (_ "," _ NamedArg)*
NamedArg <- Identifier _ ":" _ Expr

#
# Misc
#

#_ <- WS*
#__ <- WS+
_ <- (WS / Comments)*
__ <- (WS / Comments)+
WS <- [ \t\r\n]
Comments <- SingleLineComment / BlockComment
SingleLineComment <- "//" (!EOL .)* EOL?
EOL <- "\r\n" / "\n" / "\r"
BlockComment <- "/*" (!"*/" .)* "*/"
EOF <- _ !.
```
