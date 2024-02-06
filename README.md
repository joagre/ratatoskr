# The Satie programming language

## Introduction

<img src="doc/satie.png" style="width: 3em; height: auto;" align="left">
Satie is a concurrent oriented pure functional scripting language
optimized for text manipulating applications. It offers efficient
large-text buffer management with lazy loading capabilities, alongside
a suite of other features for effective handling of any-sized text
volumes.

In addition to powering text manipulating applications, Satie is also
suitable as an end-user scripting language, for writing plugins and
customizations.

However, the true strength of Satie lies in its versatility. It is a
purpose-built language and VM that boasts a high degree of generality,
making it adaptable to a wide array of applications.

Satie owes to the [Erlang](https://www.erlang.org/),
[D](https://dlang.org/) and [Scala](https://www.scala-lang.org/)
programming languages.

All rise. Here is a tribute and a premature Satie example:

```
import Std.satie
import Std.stdio : writeln

export fn main(args) {
    ?numberOfTributes := args[1],
    ?channel := satie.makeChannel(copies: numberOfTributes),
    ?jobs := startTributes(channel, numberOfTributes),
    channel.send("Standing on the shoulders of ${jobs.length} giants")
}

fn startTributes(channel, numberOfTributes, n = 0, jobs = []) {
    if n lt numberOfTributes {
        ?job := satie.spawn(fn () {
                    ?message := receive channel,
                    writeln("$n: $message")
                }),
        startTributes(channel, numberOfTributes, n + 1, job @ jobs)
    } else {
        jobs
    }
}

/*
A channel can be created with the following modifiers:

copies: N
    Keep a message in the mailbox until N instances has read it
fade: N
    If a message hasn't been read in N milliseconds it fades away
sync: Bool
    Should the sender block until the receiver has read the message
    (defaults to false)?
size: (Type, Size)
    Set the number of messages allowed in a channel to Size. If this
    threshold is reached the sender blocks or the message is ignored,
    depending on Type (which can either be the enum
    ChannelOption@block or ChannelOption@ignore).
*/
$ sac tribute.sa
$ sa build/tribute 100000
0: Standing on the shoulders of giants
1: Standing on the shoulders of giants
2: Standing on the shoulders of giants
3: Standing on the shoulders of giants
...
99999: Standing on the shoulders of giants
```
*Source: [tribute.sa](grammar/examples/tribute.sa)*

That said.

## Design choices

The following design choices have been made (in some sort of order):

### Concurrent Oriented Programming (COP)

* Satie is built on a custom multi-core enabled VM that offers robust
  support for time-sliced green threads, here after referred to as
  *jobs*. Jobs adhere to a *share-nothing* semantics and depend
  exclusively on message passing for inter-job communication. This
  architecture simplifies the process of designing and implementing
  concurrent, scalable soft real-time applications. It emphasizes
  fault tolerance and high availability, making it easier to reason
  about such systems.

* Jobs can establish *monitors* and *links* (bidirectional monitors)
  with another. This enables the creation of jobs that can supervise
  other jobs, i.e. starting and restarting them (if they unexpectedly
  fail). Supervisor jobs can also supervise other supervisor jobs, and
  so on. This makes it possible to create a tree of supervisor jobs,
  where the root supervisor job is the ultimate supervisor.

### Functional and immutable

* Satie is a pure functional and language (YMMV) featuring native
  persistent datatypes at its core. All data in Satie is immutable,
  and the persistent datatypes are custom-built to handle large
  amounts of data efficiently.

### Dynamically typed and type inference

* Satie is dynamically typed and offers a concise set of basic and
  composite types. Its compiler employs
  [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)
  type inference to deduce the types of names, expressions, and
  functions.

* Satie's dynamic type system utilizes a Garbage Collector (GC)
  mechanism to perform garbage collection on a job basis. This
  prevents the GC process from becoming a "stop-the-world" activity.

### A streamlined syntax using curly braces

* Satie, as a compact language, features a clean, regular, and
  minimalist syntax that aligns with the tradition of curly braces
  languages. This includes the use of well-known reserved words,
  syntax conventions, and scoping rules. The complete syntax is
  formally defined as a Parsing Expression Grammar (PEG) in
  [Appendix B: PEG grammar](appendix-b:-peg-grammar).

### Pattern matching in its core

Satie incorporates pattern matching as a fundamental feature.

* The `@` match operator is used to bind values to unbound names
  and/or to deconstruct composite values into their primitive
  components. There is no mutable variable assigments in Satie.

* The `switch` and `receive` expressions rely on pattern matching to
  pick the correct `case` path but also to deconstruct composite
  `case` values into their primitive components.

### A custom-built VM

* Satie operates on a custom-built VM, which includes a multi-core and
  time-slicing job scheduler. This scheduler runs multiple instances,
  one for each job, of a custom-built register machine.

* The VM and its scheduler are designed to have a small memory
  footprint. Each job, when initially started, allocates 1KB for its
  heap and stack (excluding the program code).

* The VM is standalone and has few dependencies, making it easily
  portable to systems with restricted capabilities.

### Object-orientation

* Satie has support for functional and immutable
  object-orientation. This allows for grouping of member properties
  and methods using familiar C++/Java member modifiers such as
  `public`, `private`, `const`,  and `this` (for self-referencing).

Many features are by design excluded from Satie:

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

To mention a few.

## Overall structure

A Satie file is identified by a `.sa` suffix and constitutes a Satie
*module* named after the basename of its filename. Why complicate
things? A module begins with a series of import definitions, followed
by a mix of `enum`, `interface`, `struct`, and `fn` (function)
definitions, which can be arranged in any order.

`class`, `enum`, and `interface` definitions are **only** permitted
at the top level of a module. However, function definitions can be
nested within other function definitions freely.

In an application, exactly one of the Satie modules must define a
single exported main function.

Here follows yet another premature Satie example but read it through
and an explanation will follow:

```
import Std.stdio: writeln

type Either<$t1, $t2> = Left<$t1> or Right<$t2>

enum Color {
    red
    green
    blue
}

interface Iterator<$t> {
    public fn next() is Either<false, (Iterator<$t>, $t)>
    public fn hasNext() is Bool
}

class ColorIterator implements Iterator<Color> {
    private colors
    private graffiti

    this(colors) {
        this(colors: colors, graffiti: "Kilroy was here")
    }

    public fn next() {
        if !hasNext() {
            Left<false>
        } else {
            Right<(this(colors: colors.rest()), colors.first())>
        }
    }

    public fn hasNext() {
        !colors.isEmpty()
    }
}

export fn main() {
    ?colors := [Color#red, Color#red, Color#blue, Color#green],
    ?iterator := ColorIterator(colors),
    fn iterate(iterator) {
        if iterator.hasNext() {
            (?iterator, ?color) := iterator.next(),
            writeln("Color: %color"),
            iterate(iterator)
        } else {
            true
        }
    },
    iterate(iterator)
}
```
*Source: [color.sa](grammar/examples/color.sa)*

A plain and fairly boring color iterator, and a main function that
iterates over a list of colors. Still, it manages to show the overall
structure of a Satie application.

The short story without any preamble:

A Satie module can define as many top-level functions as needed,
whether exported or not. However, it's advisable to define functions
as member functions within classes to prevent cluttering of the global
function namespace.

Only functions marked with the `export` keyword can be imported by
other modules.

`class`, `enum`, and `interface` definitions can be imported by other
modules without any restrictions though.

`<` ... `>` defines a tuple, and placing a question mark before a
name informs the compiler that the name should be treated as
unbound, regardless of its previous state. Omitting the question mark
prompts the compiler to ensure that the name is already bound. At
runtime, a check is conducted to verify that the bound name
matches the rvalue.

The `this` constructor calling itself might initially seem
confusing. However, this is the final step a constructor must perform
to actually initialize its member properties. The `:` notation is a way
to call a function with **named** parameters, and a constructor can
call itself using its member properties as named parameters. This
self-referential mechanism is an integral part of Satie's support for
functional encapsulation.

That covers the basics, but naturally, many aspects are left
unexplained at this stage.  The rest is in the gory details.

## Building and executing

The compiler is named `sac`, and the bytecode it generates can be
executed using the `sa` runner. This process was outlined in the
introduction chapter above and is reiterated here for clarity:

```
$ sac tribute.sa
$ find .
.
./tribute.sa
./build
./build/tribute.sab
$ sa build/tribute 100000
0: Standing on the shoulders of giants
1: Standing on the shoulders of giants
2: Standing on the shoulders of giants
3: Standing on the shoulders of giants
...
99999: Standing on the shoulders of giants
```

That is it.

An application can also be composed of a multitude of modules within a
directory hierarchy, but the build process remains the same. For
instance, consider an application named `zarah` consisting of these
modules:

```
$ cd zarah
$ find .
.
./src
./src/main.sa
./src/utils
./src/utils/httpclient.sa
./src/database
./src/database/backup.sa
./src/database/utils
./src/database/tablestore.sa
```

Here is how the application can be compiled and executed:

```
$ sac src/main.sa
$ find .
.

./build
./build/main.sab
./build/utils
./build/utils/httpclient.sab
./build/database
./build/database/utils
./build/database/backup.sab
./build/database/tablestore.sab
./src
./src/main.sa
./src/utils
./src/utils/httpclient.sa
./src/database
./src/database/backup.sa
./src/database/utils
./src/database/tablestore.sa
$ sa build/main
```

**Note:** The `sac` compiler can be configured  to use alternative
directories for `src/` and `build/`.

By default, the `sac` compiler follows module dependencies starting
from `main.sa` and automatically compiles those modules as
well. However, it can be configured not to follow these dependencies,
to ignore missing modules, or to skip modules that cannot be
compiled. For more detailed information about the `sac` compiler and
the `sa` runner, refer to their respective manual pages.

## The interactive shell

The `sa` runner is also capable of launching an interactive shell:

```
$ sa --shell
Satie <0.3.1> (press ctrl-c to abort)
0> _
```

In the shell expressions can be evaulated and the status of an already
executing application can be inspected, i.e a shell can be made to
connect to an already executing `sa` runner instance. Read more about
this in the `sa` runner's manual page.

# The gory details

## Comments

Everything after `//` and to end of line and within `/* ... */` are
considered comments.

## Types

### Basic types

`bool` : A boolean

`int` : A signed integer

`float` : A floating-point

`char` : A 32-bits Unicode code point

`enum`: An enumeration

`function` : A function

`job` : A job

`channel` : A mailbox channel

### Composite types

`string` : An immutable sequence of UTF-8 encoded characters

`tuple` : A fixed sized sequence of values of any type

`list`: A list of values of any type

`map`: A mapping between a key of any type and a value of any type

`object` : A unit of encapsulation for member properties and member
methods

`buf` : A buffer to efficiently manipulate large amount of characters

### Type management

All values can be type checked in run-time using the functions
`isBool`, `isInt`, `isFloat`, `isChar`, `isFunction`, `isJob`,
`isEnum`, `isString`, `isList`, `isMap`, `isObject`, `isBuf` and
`typeof`.

Look at these examples:

```
?a = 3.14,
a.isInt(),      // false
a.isFloat(),    // true
a.typeof()      // "float"
```

Operators that operate on `int` and `float` values require the
operands to be of the same type. No implicit numeric conversion is
performed. However, a cast operator is available to facilitate casting
between int and float values.

Grok this:

```
?a = 3,
?b = 93326215443944152681,
?c = 3.0,
?d = b / cast(int)c + a,     // 31108738481314713603
d + c                        // Error!
```

The `inspect` function offers more run-time type information:

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
                    //   "owner" : <"bar", "Foo"> ]
d.inspect()         // [ "type" : "list", "length": 2 ]
```

**Note:** In the above example the value attached to the enumeration
constant would typically be accessed using `c.value`, and the list
length would be referred to as `d.length`.

All values can be converted to string representation using the
`toString` function.

Try this:

```
?a = 3.14,
?b = [Foo.a : 42, "bar": <fn () { x + x}, [1, 2,3]>]
a.toString(),       // "3.14"
b.toString()        // "[Foo.a : 42, "bar": fn/0]"
```

## Identifiers

An identifier is a case-sensitive sequence of characters that begins
with an ASCII letter or an underscore, followed by any number of ASCII
letters, underscores, or digits. This is denoted by the regular
expression `^[[:alpha:]_][[:alnum:]_]*$`. Identifiers are used for
various elements such as keywords, names, function names, class
names, and enum names.

**Note:** By design, only strings may contain Unicode
characters. However, this restriction could be reconsidered and
potentially lifted if compelling reasons arise.

### Keywords

29 special identifiers cannot be used in user code. These reserved
identifiers are exclusive to the language's internal syntax and
functionality.

```
import
true
false
enum
fn
export
if
else
switch
default
in
is
as
class
new
interface
public
private
readonly
const
this
receive
timeout
self
```

(no nil!)

Operators are normally not identifiers but the following reserved
symbolic operators are exceptions:

````
gt
gte
lt
lte
bsl
bsr
```

The reason is that the tuple syntax uses a '<' .. '>' for
bracketing. It seemed like an awful waste to use the bracketing
characters '<' and '>' as plain operators. Tuples are very common in
Satie code. These operators less so.

## Literals

### Boolean literal

`true` or `false`

### Integral literal

An integral literal can be represented as decimal, octal and
hexadecimal values:

```
4,             // Decimal format
017,           // Octal format
0xffff,        // Hexadecimal format
0b101010100    // Binary format
```

### Floating-point literal

A floating point literal represents a real number that includes a
fractional part. Its notation is similar to that used in other
languages like C or Java:

```
1.0,
.666e2
```

### Character literal

A character literal is a Unicode code point value enclosed within
single quotation marks. Internally, it consists of four bytes:

```
'A',
'ω',
'\u03c9'    // ω
```

### Function literal

A function literal follows the same syntax as regular [function
definitions](function), but without a function name:

```
?sum = fn (x, y) { x + y },
sum(1, 2)                   // 3
```

### Job literal

Job literals are opaque.

### Enumeration literal

An enumeration is a named constant, and it is always defined
within a [enumeration definition](enumeration). An enumeration literal
is represented as a dot-separated sequence, consisting of the
enumeration name followed by the constant name:

```
enum Color {
    red
    green
    blue
}

Color.red                   // An enumeration literal
```

### String literal

A string literal is represented as an immutable UTF-8 encoded sequence
of Unicode characters, enclosed within double quotation marks. Escape
sequences are interpreted in these double quoted
strings. Additionally, there is raw strings, which are also enclosed
within double quotation marks but prefixed with the letter `r`. In raw
strings, escape sequences have no meaning, and all characters are
parsed verbatim.

Like this:

```
?a = "fooω",
a[3],               // 'ω'
?b = r"foo\nbar"    // b.length == 8
```

### Tuple literal

A tuple literal is represented as comma separated, fixed size sequence
of values of any type. This sequence is enclosed between a leading
`<` and a trailing `>`:

`<"foo", 3.14, <"bar", fn (x) { x + 1}>>`

### List literal

A list literal is represented as comma-separated sequence of values,
which can be if any type enclosed within square brackets:

```
?a = [3.14, "foo", 1816381],
?b = a[1 = 42, 2 = "bar"]      // b == [3.14, 42, "bar"]
```

**Note:** Only existing list entries can be updated this way.

### Map literal

A map literal is represented as comma-separated sequence of key-value
pairs, where the key and value can be of any type and are separated by
a `:` character. This sequence is enclosed within square brackets:

```
?a = ["foo" : 12, 3.14 : 981237198192378 ],
a[3.14: 4711, 2 : 4]                         // ["foo" : 12, 3.14: 4711, 2 : 4],
```

## Expressions

In Satie every element is treated as an expression, with the exception
of top-level definitions. These include `import`, `class`,
`interface`, and `enum`.

Among all expressions, the match expression, marked by the `@`
operator, stands out for its ability to bind values to names. An
unbound name is identified by a leading `?` character, and a naked
name (without a `?`) must be bound before being refered.

Like this:

```
?a = 42,
a = 42,
a = "foo",     // Error!
b = 4711       // Error!
```

In the example above, `a = "foo"` leads to an error, where `a`
is expected to be `42`, not `"foo"`. Such mismatches are typically
used as assertions or may indicate a software bug. A runtime error
halts the execution of the job where the mismatch occurred. For more
details on concurrency and jobs, refer to the
[Concurrency](concurrency) section.

Additionally, in the example above, `b = 4711` would result in a
compiler error because `b` has not been previously bound.

The pattern matching capability of bind expressions can be utilized
to deconstruct composite values into their primitive components.

Grok this:

```
fn foo(x) {
  <4711, x + x, "bar">
},
?a = 1,
<a, ?b, 1> = <1, 2, 1>,     // b == 2
<?a, b, ?c> = foo(1),       // a == 4711 && c == "bar"
<a, a, a> = c               // Error!
```

Pretty nifty.

**Note:** In the examples above, we used expressions that have not been
explained yet. For now, it's sufficient to understand that `<1, 2, 1>`
represents a fixed-size tuple.

The `switch` and `receive` expressions, which are described below,
also employ pattern matching. This feature enables them to deconstruct
and select specific elements. Further details about these expressions can
be found in the subsequent sections.

Furthermore, the `@` operator is restricted to be used as a
standalone expression and is not permitted within more complex
expressions. This restriction is in place to maintain clarity and
simplicity in coding practices. Your understanding and cooperation are
appreciated. 😇

This produces an error:

```
main() {
    ?a = 42,
    a + (?c = 42 + c) + a    // Error!
}
```

### Block expression -- `{` a, b, c, ... `}`

The block expression consists of a sequence of expressions separated by
commas and enclosed in curly braces. These expressions are evaluated
sequentially and establish a lexical scope. A name bound within
this scope is visible to all subsequent expressions in the same
scope. However, it remains invisible outside of this scope and
overshadows any identifier with the same name introduced outside the
scope. The value of the last expression in the sequence is what the
block ultimately returns.

Take a look at this:

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

### If expression -- `if`, `elif`, `else`

The `if` expression is straightforward and does not require additional
clarification:

```
?a = 4,
?b = if a == 4 {
         42
     } elif {
         c,
         d
     },
b         // 42
```

### Switch expression -- `switch`, `case`, `default`

The `switch` expression employs pattern matching not only to dispatch
between its case paths but also to perform deconstruction, as
introduced in the [Expressions](expressions) section.

A simple case path selection:

```
?a = 42,
switch a {
    case "foo" {
        "No!"
    }
    case 42 {
        "Yes!"
    }
}
```

A combined case path selection and tuple deconstruction:

```
?a = <"bar", 4711>,
switch a {
    case "foo" {
        "Darn!"
    }
    case <"bar", ?c> {
        c
    }
    default {
        "No one picks me!"
    }
}
```

No more no less.

**Note:** The `switch` expression does not include a fall-through
mechanism, and the use of the `default` keyword is optional. The
absence of switch fall-through makes the world a little bit
better. YMMV.

### Enumeration -- `a.b`

The enumeration definition introduces named constants, improving code
readability and maintainability. A named constant can optionally have
a value of any type.

Like this:

```
enum Color {
    red = <255, 0, 0>
    green = <0, 255, 0>
    blue = <0, 0, 255>
}

Foo.red,              // An enumeration literal
Foo.red.value         // <255, 0, 0>
```

### Function -- `fn`

`foo(a, b)` calls the function `foo` with a list of expression
arguments separated by commas. These arguments are evaluated from left
to right before invoking the function. `foo` can refer to either a
named function definition or a name bound to a function
literal. Functions can be overloaded, and their parameters can have
default values.

Grok this:

```
fn foo(a, b, c = 0) {
  c;
  d;
}

fn foo(a = 1) {
  a;
}
```

Default values are assignable only to trailing parameters, and such
parameters (as seen in the example above) can be omitted in function
calls. Furthermore, a function call must use either positional
parameters exclusively or named parameters exclusively. The following
function calls illustrate equivalent function calls:

```
foo(2, 6),
foo(2, 6, 0),
foo(a: 2, b: 6),
foo(b: 6, a: 2)
```

Functions can be defined within functions:

```
fn foo(a, b, c = 0) {
    fn bar(d) {
        d
    },
    bar(a)
}
```

Functions are treated as first-class citizens:

```
import stdio.lists

export fn main() {
    ?l = [1, 3, 2],
    ?f = fn (x, y) { x > y },
    lists.sort(l, f)
}
```

### String -- `"` ... `"`

A string is an immutable sequence of UTF-8 encoded characters. String
interpolation is supported, and despite the UTF-8 encoding, random
access to individual characters in a string is possible.

Grok this:

```
?a = 3.0,
?b = "foo $a is not ${a + 1.0}"  // b == "foo 3.0 is not 4.0" (interpolation)
?a = "foo",
?b = "bar",
?c = a ~ b,                     // c == "foobar"
?c = a ~ '\u03c9'               // c == "fooω"
c[3] == 'ω'                     // true
r"foo\nbar"                     // A raw string
r.length == 9                   // true
```
### Tuple -- `<` a, b, c, ... `>`

Not much needs to be said about tuples: they are simply tuples.

Like this:

```
?a = 42,
?b = <4711, <a, [1, 2]>>,
<_, <?a, [_, c]>> = b,
a,                           // 4711
c                            // 2
```

### List -- `[` a, b, c, ... `]`

A list can hold elements of any type, and list slicing makes it easy
to work with portions of a list. The expression `a[i .. j]` returns a
slice of the list starting at index `i` and ending at the index
`j - 1`. Both `i` and `j` can be any valid expression, and the keyword
`$` represents the list's length.

Grok this:

* `a[i]` returns the i:th element.
* `a[2 = "foo"]` evaluates to a new list with element 2 set to "foo".

Study this:

```
?a = [],                 // An empty list
?a = [1, 2, 3, 4, 5],    // A rebind of name a
a.first(),               // 1
a.rest(),                // [2, 3, 4, 5]
?b = a[1 .. 3],          // b == [2, 3, 4]
b.length == 3,           // true
?c = a[2 .. $ - 1],      // c == [3, 4, 5]
?d = b ~ c,              // d == [2, 3, 4, 3, 4, 5]
d[1 = 42],               // [2, 42, 4, 3, 4, 5]
a[2 = 23],               // [1, 2, 23, 4, 5],
?b = 4711 ~ b,           // b == [4711, 2, 3, 4]
?f = a[$ / 2 .. $ - 1]   // f == [3, 4, 5],
f.delete(2),             // [3, 4]
?g = a,
g == a                   // true
```

### Map  -- `[` a `:` b, ... `]`

A map can be viewed as a function that creates a mapping between keys
and values, both of which can be of any type.

Grok this as well:

```
?a = [:],                      // En empty map
?a = ["a": 1.0, "b": "foo"],
a["a": "bar"],                 // ["a": "bar", "b": "foo"],
a[42 : 4711],                  // ["a": "bar", "b": "foo", 42: 4711],
?c = a ~ ["pi": 3.14]          // c == [ "a" : 1.0, "b" : "foo", "pi" : 3.14 ]
c.length == 3,                 // true
a.delete("a"),                 // [ "b" : "foo" ]
a.keys,                        // ["a", b] || ["b", "a"]
a["a"],                        // 1.0
a.values                       // [1.0, "foo"] || ["foo", 1.0]
?d = ["a": 1.0, "b": "foo"],   //
a == d                         // true
```

### Encapsulation -- `class` a `{` ... `}`

A class in Satie goes beyond a traditional C++ class. It is immutable
and inherently functional, serving as a unit of encapsulation for
member properties and methods.

Lets dive into an example right away:

```
class Foo {
    public a = 1
    private b = 2
    readonly c = 3
    public const d = 4
    private const e = 5

    this(a, g) {          // Optional constructor
        this(a: a, b: g)
    }

    ~this() {             // Optional destructor
        // Do nothing for now
    }

    public fn foo(a) {
        this.a + b;
    }

    private fn bar(b) {
        b + d
    }
}
```

The example above revisits familiar concepts from Java, including
member properties and methods, access specifiers and modifiers, along
with a constructor and destructor, and the use of the this
reference. No novel elements have been introduced here.

Interestingly, the constructor is designed to call itself for the
final initialization of its member properties. It's noteworthy how the
constructor can use its member properties as named parameters in this
self-referential call.

To instantiate a class `Foo` do this:

`?a = new Foo(2, 1)`

A class can opt to implement certain mandatory interfaces. An
*interface* specifies the member properties and functions that the
class must provide. The structure of an interface definition is as
follows:

```
interface Bar {
    public zippo
    public fn bonk()
}
```

A class that opts to implement this interface is structured as
follows:

```
class Foo : Bar {
    public zippo = 8
    public fn bonk() {
        // Delve into the nature of God
    }
    ...
}
```

A class can implement several interfaces using a comma separated
sequence of interfaces:

```
class Foo : Bar, Bonk {
    ...
}
```

Do the following to define a set of constants:

```
class Math {
    public const PI = 3.1;
    public const SQUARE2 = math.sqrt(2);
}
```

**Note:** This class must be instantiated somewhere in the code as
there is no `static` modifier in Satie.

### Buf(fer) -- `buf`

A buffer is an opaque, persistent data type designed for the efficient
manipulation of large amounts of characters. It is well-suited for a
programming language aimed at simplifying the implementation of
programming editors and more.

**Note:** This section is currently under consideration and the
details are not yet finalized.

API overview:

1. Creation and Initialization

   * `create()`: Initializes an empty buf.
   * `fromString(string)`: Creates a buf from a given string.
   * `fromFile(filePath, lazyLoad)`: Initializes a buf from a file,
     with optional lazy loading for large files.

1. Reading and Access
   * `charAt(index)`: Returns the character at a specified index.
   * `substring(startIndex, endIndex)`: Retrieves a substring from the
     buffer.
   * `length()`: Provides the length of the text in the buffer.

1. Text Modification
   * `insert(index, string)`: Inserts a string at the specified index.
   * `delete(startIndex, endIndex)`: Deletes text between given indices.
   * `replace(startIndex, endIndex, string)`: Replaces a segment of
     text with a new string.

1. Text Selection and Clipboard Operations
   * `select(startIndex, endIndex)`: Selects text between given indices.
   * `cut(startIndex, endIndex)`: Cuts (removes and copies) the
   selected text.
   * `copy(startIndex, endIndex)`: Copies the selected text.
   * `paste(index, string)`: Pastes the copied text at the specified index.

1. Search and Navigation
   * `indexOf(substring, startIndex)`: Finds the index of the first
     occurrence of a substring.
   * `lastIndexOf(substring, startIndex)`: Locates the last occurrence
     of a substring.
   * `moveCursorTo(index)`: Moves the cursor to a specified index for
     navigation purposes.

1. Text Transformation
   * `toUpperCase()`: Converts all text in the buffer to uppercase.
   * `toLowerCase()`: Converts all text in the buffer to lowercase.
   * `map(function)`: Applies a specified function to each character
     in the buffer.

1. Undo Mechanism
   * `undo()`: Reverts the buffer to its previous state, utilizing a
     history of operations or states.

1. Concurrency
   * `asyncInsert(index, string)`: Asynchronously inserts a string at
     a specified index.
   * `asyncDelete(startIndex, endIndex)`: Asynchronously deletes text
     between given indices.
   * `asyncReplace(startIndex, endIndex, string)`: Asynchronously
     replaces text in a specified range.

1. Utility Functions
   * `toString()`: Converts the buf to a standard string for output or
      display.
   * `serialize()`: Serializes the buf for storage or transmission.
   * `deserialize(serializedData)`: Constructs a buf from serialized data.

1. Advanced Editing
   * `batch(operations)`: Performs multiple operations in a single
     step for efficiency.

1. File Handling and Lazy Loading
   * `loadMore()`: Incrementally loads more content from the file if
     lazy loading is enabled.

1. Pending additions
   * `isEmpty()`: Checks if the buffer is empty.
   * `trim()`: Removes whitespace from the beginning and end of the text.
   * `split(separator)`: Splits the buffer into a list of bufs based
     on a separator.
   * `merge(buffers)`: Combines multiple buf instances into one.

Design Considerations:
* *Immutability*: Each operation creates a new buf instance, preserving
the original and adhering to the principles of functional programming.
* *Efficiency and Scalability*: The design is optimized for common text
editing operations, with considerations for handling large files
through lazy loading.
* *Clipboard Operations*: The addition of select, cut, copy, and paste
functionalities provides essential editing capabilities.
* *Extensibility*: The design allows for future extensions, such as
language-specific features or plugins.
* *Undo Mechanism*: The undo functionality is natural and integral,
implemented via a history of states or operations.
* *Concurrency Support*: Concurrency support is vital for
collaborative editing scenarios, requiring thread-safe operations and
potential conflict resolution strategies.
* *Lazy Loading*: This feature enables efficient handling of large
files, loading content as needed rather than all at once.

With these functionalities and considerations, the `buf` datatype
becomes a comprehensive and robust tool for building a programming
editor, offering a wide range of functionalities required for text
editing and manipulation in a functional programming environment.

## Concurrency

Any function can be executed concurrently as a job using the `spawn`
keyword.

Grok this:

```
fn sum(l) { l.first() + sum(l.rest() },
?a = [1, 2, 3],
job = spawn sum(a)
```

In the example above, `spawn` returns a job reference and it can be
used to send messages to the job using the `#` operator:

```
job # <timeout, 1000>
```

A message sent to a job is placed in its mailbox and can be retrieved
using the receive keyword.

Like this:

```
receive {
    case <?job, ?result> {
        stdio.writeln("Job $job sent result $result")
        result
    }
    timeout 1000 {
        42
    }
}
```

**Note:** The `timeout` keyword is optional and the timeout unit is in
milliseconds.

The mailbox is unbounded in size but can be restricted using the
`std.concurrency` library function `setMaxMailboxSize`:

Like this:

`concurrency.setMaxMailboxSize(job, 64, OnCrowding.block)`

In the example above the job's mailbox is restricted to contain at
most 64 messages, and if a sending job hits that threshold it is
automatically blocked in the `#` operator waiting for the mailbox to
shrink.

As an alternative, `OnCrowding.ignore` can be used to specify that
overflowing messages should be ignored.

The `OnCrowding` constant can also be replaced with a function that
returns `false` if overflowing messages should be ignored or `true` if
the sending job should be blocked in the `#` operator.

The  concurrency keyword `self`  refers to the job which user code
currently runs within.

The `std.concurrency` module also provides these functions:

* `monitor(job)` : Send a message `<JobMonitor.died, job, reason>` to
me if a job dies
* `link(job)` : Send a message `<JobMonitor.died, job, reason>` to me
if a job dies. Do the same to the linked job if I die. A link is a
bidirectional monitor.
* `kill(job)`

The `spawn` keyword can take an extra `monitor` or `link` specifier to
create a monitor or link during the actual spawn. Look how this is
done in the upcoming example.

### A small concurrency example

Here follows a small concurrent example that spawn jobs to compute
Ackermann function values for the parameters `m = 3, n = 1 .. 10`.

The `main` function uses member functions in an Ackermann class to
start 10 jobs and then waits for all jobs to send a result back as a
message.

Study this and do not despair:

```
import Std.satie
import Std.jobs: JobStatus, SpawnOption
import Std.channels: ChannelOption
import Std.stdio

export fn main(_args) {
  ?ackermann := Ackermann(),
  ?ackermann := ackermann.startJobs(3, 10),
  ackermann.waitForJobs()
}

class Ackermann {
    private jobs is [Job] = []
    private resultChannel is Channel

    this() {
        this(resultChannel:
                 satie.makeChannel(onCrowding: (ChannelOption#block, 4)))
    }

    public fn startJobs(m, n, i = 0, startedJobs = []) {
        if i lt n {
            fn computeAckermann(m, n) {
                ?result := ackermann(m, n),
                resultChannel.send((self, m, n, result))
            },
            ?job := satie.spawn(fn () {
                        computeAckermann(m, i)
                    }, [SpawnOption#monitor: true]),
            startJobs(m, n, i + 1, job @ startedJobs)
        } else {
            this(jobs: startedJobs)
        }
    }

    public fn waitForJobs() {
        fn waitForJobs(jobs) {
            if jobs.length gt 0 {
                receive [systemChannel, satie.systemChannel] {
                    (?job, ?m, ?n, ?result) {
                        stdio.writeln("ackermann(%m, %n) = %result"),
                        waitForJobs(jobs.delete(job))
                    }
                    (JobStatus#died, ?job, ?reason) {
                        stdio.writeln("Oh no! Compute job %job died: %reason"),
                        waitForJobs(jobs.delete(job))
                    }
                }
            } else {
                this(jobs: [])
            }
        },
        waitForJobs(jobs)
    }

    private fn ackermann(m, n) {
        if m =Int= 0 {
            n + 1
        } elif n !Int= 0 {
            ackermann(m - 1, 1)
        } else {
            ackermann(m - 1, ackermann(m, n - 1))
        }
    }
}
```
*Source: [ackermann.sa](grammar/examples/ackermann.sa)*

## Directory hierarchy of modules

A directory hierarchy of modules is a nice way to organize code and in
the example below a module uses the `foreach` and ` writeln` functions
from the standard libray module hierarchy:

```
import std.stdio
import std.lists

export fn main(args) {
  lists.foreach(fn (arg) { stdio.writeln("$arg") }, args)
}
```

Note how the module names must be specified in the call to  the call
to `foreach` and `writeln`, i.e. nothing is automatically imported
into the module namespace.

It is possible to import enumerations, interfaces, classes and
functions into a module namespace:

```
import std.stdio : writeln
import std.lists : foreach

export fn main(args) {
  foreach(fn (arg) { writeln("$arg") }, args)
}
```

In the [Building and Executing](building-and-executing) section above
the *zarah* project was introduced and it had the following directory
hierarchy of modules:

```
$ cd zarah
$ find .
.
./src
./src/main.sa
./src/utils
./src/utils/httpclient.sa
./src/database
./src/database/backup.sa
./src/database/utils
./src/database/tablestore.sa
```

The modules `std.lists` and `database.utils.lists` have the same
module name. To resolve this, an import alias can be used.

Like this:

```
import std.stdio : writeln
import std.lists
import dlists = database.utils.lists
import database

export fn main(args) {
  lists.foreach(fn (arg) { writeln("$arg") }, args)  // as before
  dlists.removeReference(fn (staleReference) {
      database.removeReference(staleReference)
  }, database.getStaleReferences())
}
```

That is it.

# Appendix A: Operator precedences

Operators in decreasing order of precedence:

| Expression | Description                              |
|------------|------------------------------------------|
| a.b        | Enum access                              |
| a.b(c)     | Method access                            |
| a(b, c)    | Function call                            |
| a[i]       | List and string indexing                 |
| a[b .. c]  | List slicing  (see "List Literal" above) |
| a[a = b]   | List setter (see "List Literal" above)   |
| a[a : b]   | Map setter (see "Map Literal" above)     |
| -a         |                                          |
| +a         |                                          |
| !a         |                                          |
| ~a         | Bitwise complement                       |
| #          | Send message                             |
| cast(t)a   | Cast expression                          |
| a ^^ b     | Exponentiation                           |
| a * b      |                                          |
| a / b      |                                          |
| a % b      | Modulus                                  |
| a + b      |                                          |
| a - b      |                                          |
| a ~ b      | Concatenation                            |
| a :<< b    |                                          |
| a :>> b    |                                          |
| a in b     | Map membership                           |
| a == b     | Equality                                 |
| a :< b     |                                          |
| a :<= b    |                                          |
| a :> b     |                                          |
| a :>= b    |                                          |
| a \| b     |                                          |
| a ^ b      | Bitwise xor                              |
| a & b      |                                          |
| a && b     | Logical and                              |
| a \|\| b   |                                          |
| a @ b      | Match                                    |

# Appendix B: PEG grammar

```
%prefix "satie"

%header {
    #include <stdarg.h>
    #include "satie_auxil.h"
    #include "symbol_table.h"
    #include "hm.h"

/*
    static const char *dbg_str[] = { "Evaluating rule", "Matched rule", "Abandoning rule" };
    #define PCC_DEBUG(auxil, event, rule, level, pos, buffer, length) \
    if (strcmp(rule, "WS") != 0 && strcmp(rule, "_") != 0 && strcmp(rule, "__") != 0) \
        fprintf(stderr, "%*s%s %s @%zu [%.*s]\n", (int)((level) * 2), "", dbg_str[event], \
                rule, pos, (int)(length), buffer)
*/

    static int ROW = 1;

    static int satie_getchar(satie_auxil_t* _auxil) {
        int c = getchar();
        if (c == '\n') {
            ROW++;
        }
        return c;
    }

    static void panic(const char *fmt, ...) {
        va_list args;
        va_start(args, fmt);
        fprintf(stderr, "\033[31mError:\033[0m ");
        vfprintf(stderr, fmt, args);
        fprintf(stderr, "\n");
        va_end(args);
        exit(1);
    }

    static void satie_error(satie_auxil_t* auxil) {
        panic("Bailing out near line %d", ROW);
        exit(1);
    }
}

%auxil "satie_auxil_t*"
%value "ast_node_t*"

%source {
#define PCC_GETCHAR(auxil) satie_getchar(auxil)
#define PCC_ERROR(auxil) satie_error(auxil)
}

#
# Top level structure
#

Program <- _ (i:Imports __)? t:TopLevelDefs EOF { $$ = CN(PROGRAM, 2, i, t); }
TopLevelDefs <- t:TopLevelDef { $$ = CN(TOP_LEVEL_DEFS, 1, t); } (__ t:TopLevelDef { AC($$, t); } / TopLevelDefError)*
TopLevelDef <- (t:AliasDef / t:TypeDef / t:EnumDef / t:FunctionDef / t:InterfaceDef / t:ClassDef) { $$ = t; }
TopLevelDefError <- ("," / ";") {
    panic("Unexpected %s between top level definitions on line %d",
          $0, auxil->row);
}

#
# Import
#

Imports <- i:Import { $$ = CN(IMPORTS, 1, i); } (__ i:Import { AC($$, i); })*
Import <- "import" __ (ma:ModuleAlias _ "=" _)? _ m:ModuleComponents (_ ":" _ i:ImportedNames)? { $$ = CN(IMPORT, 3, ma, m, i); }
ModuleAlias <- Identifier { $$ = CT(MODULE_ALIAS, $0); }
ModuleComponents <- m:ModuleComponent {$$ = CN(MODULE, 1, m); } ( "." m:ModuleComponent { AC($$, m); })*
ModuleComponent <- Identifier { $$ = CT(MODULE_COMPONENT, $0); }
ImportedNames <- m:ImportedName { $$ = CN(IMPORTED_NAMES, 1, m); } (_ "," _ m:ImportedName { AC($$, m); })*
ImportedName <- Identifier { $$ = CT(IMPORTED_NAME, $0); }

#
# Alias definition
#

AliasDef <- "alias" _ n:Name ("<" _ tv:TypeVariables _ ">")? _ "=" _ t:Type { $$ = CN(ALIAS_DEF, 3, n, tv, t); }

#
# Type definition
#

TypeDef <- "type" _ n:Name ("<" _ t:TypeVariables _ ">")? _ "=" _ tc:TypeConstructors { $$ = CN(TYPE_DEF, 3, n, t, tc); }
TypeConstructors <- t:TypeConstructor { $$ = CN(TYPE_CONSTRUCTORS, 1, t); } (_ "or" _ t:TypeConstructor { AC($$, t); })*
TypeConstructor <- n:Name ("<" _ t:Types _ ">")? { $$ = CN(TYPE_CONSTRUCTOR, 2, n, t); }

#
# Enumeration definition
#

EnumDef <- "enum" __ ed:EnumDefName _ "{" _ e:Enums _ "}" { $$ = CN(ENUM_DEF, 2, ed, e); }
EnumDefName <- Identifier { $$ = CT(ENUM_DEF_NAME, $0); }
Enums <- e:Enum { $$ = CN(ENUMS, 1, e); } (__ e:Enum { AC($$, e); } / EnumValueError)*
Enum <- en:EnumName (_ "=" _ e:Expr)? { $$ = CN(ENUM, 2, en, CN(ENUM_VALUE, 1, e)); }
EnumName <- Identifier { $$ = CT(ENUM_NAME, $0); }
EnumValueError <- ("," / ";") {
    panic("Unexpected %s between enum values on line %d", $0, auxil->row);
}

#
# Function definition
#

FunctionDef <- e:Export? _ "fn" __ f:FunctionName ("<" _ tv:TypeVariables _ ">")? _ "(" _ p:Params? _ ")" _ ("is" _ rt:Type _)? b:BlockExpr { $$ = CN(FUNCTION_DEF, 6, e, f, tv, p, CN(RETURN_TYPE, 1, rt), b); }
Export <- "export" { $$ = CT(EXPORT, NULL); }
FunctionName <- Identifier { $$ = CT(FUNCTION_NAME, $0); }
Params <- n:NonDefaultParams _ "," _ d:DefaultParams { $$ = CN(PARAMS, 2, n, d); } /
          n:NonDefaultParams { $$ = n; } /
          d:DefaultParams { $$ = d; }
NonDefaultParams <- n:NonDefaultParam { $$ = CN(NON_DEFAULT_PARAMS, 1, n); }
                    (_ "," _ n:NonDefaultParam { AC($$, n); })*
NonDefaultParam <- n:Name (_ "is" _ t:Type)? !(_ "=") { $$ = RN(n, PARAM_NAME); AC($$, t);}
DefaultParams <- d:DefaultParam { $$ = CN(DEFAULT_PARAMS, 1, d); } (_ "," _ d:DefaultParam {AC($$, d); })*
DefaultParam <- i:DefaultParamName (_ "is" _ t:Type { AC(i, t); })? _ "=" _ m:ParamExpr { $$ = CN(DEFAULT_PARAM, 2, i, m); }
DefaultParamName <- Identifier { $$ = CT(PARAM_NAME, $0); }
ParamExpr <- (m:MatchLiteral / m:Name) { $$ = m; }
BlockExpr <- "{" _ b:BlockLevelExprs _ "}" { $$ = b; }
BlockLevelExprs <- b:BlockLevelExpr { $$ = CN(BLOCK_EXPR, 1, b); } (_ Comma _ b:BlockLevelExpr { AC($$, b); })*
BlockLevelExpr <- (b:FunctionDef / b:Expr) { $$ = b; }
Comma <- "," / ";" {
    panic("Unexpected ';' on line %d (use ',' as a separator between "
          "expressions)", auxil->row);
}
Args <- (a:PositionalArgs / a:NamedArgs) { $$ = a; }
PositionalArgs <- !NamedArg e:Expr { $$ = CN(POSITIONAL_ARGS, 1, e); } (_ "," _ e:Expr { AC($$, e); })*
NamedArgs <- n:NamedArg { $$ = CN(NAMED_ARGS, 1, n); } (_ "," _ n:NamedArg { AC($$, n); })*
NamedArg <- n:Name _ ":" _ r:Expr { $$ = CN(NAMED_ARG, 2, n, r); }

#
# Interface definition
#

InterfaceDef <- "interface" __ i:InterfaceName ("<" _ t:TypeVariables _ ">")? _ "{" _
                 im:InterfaceMembers _
                 "}" { $$ = CN(INTERFACE_DEF, 3, i, t, im); }
InterfaceName <- Identifier { $$ = CT(INTERFACE_NAME, $0); }
InterfaceMembers <- i:InterfaceMember { $$ = CN(INTERFACE_MEMBERS, 1, i); } (_ i:InterfaceMember { AC($$, i); } / InterfaceMemberError)*
InterfaceMember <- (i:InterfaceConstructor / i:InterfaceDestructor / i:InterfaceMemberMethod / i:InterfaceMemberProperty) { $$ = i; }
InterfaceConstructor <- "this" _ "(" _ p:Params? _ ")" (_ "is" _ t:Type)? { $$ = CN(INTERFACE_CONSTRUCTOR, 2, p, t); }
InterfaceDestructor <- "~this" _ "(" _ p:Params? _ ")" (_ "is" _ t:Type)? { $$ = CN(INTERFACE_DESTRUCTOR, 3, p, t); }
InterfaceMemberMethod <- m:MemberAccess _ i:InterfaceMethod { $$ = CN(INTERFACE_MEMBER_METHOD, 2, m, i); }
InterfaceMethod <- "fn" _ f:FunctionName ("<" _ tv:TypeVariables _ ">")? _ "(" _ p:Params? _ ")" _  ("is" _ t:Type _)? { $$ = CN(INTERFACE_METHOD, 4, f, tv, p, t); }
InterfaceMemberProperty <- ((m:MemberAccess (_ c:Const)? / c:Readonly) _ n:Name  (_ "is" _ t:Type { AC(n, t); })?) { $$ = CN(INTERFACE_MEMBER_PROPERTY, 4, m, c, n, t); }
InterfaceMemberError <- ("," / ";") {
    panic("Unexpected %s between members on line %d", $0, auxil->row);
}

#
# Class definition
#

ClassDef <- "class" __ cn:ClassName ("<" _ t:TypeVariables _ ">")? _ ( "implements" _ i:Interfaces _ )? "{" _
            c:ClassMembers _
            "}" { $$ = CN(CLASS_DEF, 4, cn, t, i, c); }
ClassName <- Identifier { $$ = CT(CLASS_NAME, $0); }
Interfaces <- i:Interface { $$ = CN(INTERFACES, 1, i); } (_ "," _ i:Interface { AC($$, i); })*
Interface <- n:Name ("<" _ t:Types _ ">")? { $$ = CN(INTERFACE, 2, n, t); }
ClassMembers <- c:ClassMember { $$ = CN(CLASS_MEMBERS, 1, c); } (_ c:ClassMember { AC($$, c); } / ClassMemberError)*
ClassMember <- (c:Constructor / c:Destructor / c:MemberMethod / c:MemberProperty) { $$ = c; }
Constructor <- "this" _ "(" _ p:Params? _ ")" (_ "is" _ t:Type)? _ b:BlockExpr { $$ = CN(CLASS_CONSTRUCTOR, 3, p, t, b); }
Destructor <- "~this" _ "(" _ p:Params? _ ")" (_ "is" _ t:Type)? _ b:BlockExpr { $$ = CN(CLASS_DESTRUCTOR, 3, p, t, b); }
MemberMethod <- m:MemberAccess _ f:FunctionDef { $$ = CN(MEMBER_METHOD, 2, m, f); }
MemberAccess <- "public" { $$ = CT(PUBLIC, NULL); } / "private" { $$ = CT(PRIVATE, NULL); }
MemberProperty <- ((m:MemberAccess (_ c:Const)? / c:Readonly) _ n:Name (_ "is" _ t:Type { AC(n, t); })? (_ "=" _ e:Expr)?) { $$ = CN(MEMBER_PROPERTY, 4, m, c, n, e); }
ClassMemberError <- ("," / ";") {
    panic("Unexpected %s between members on line %d", $0, auxil->row);
}
Const <- "const" { $$ = CT(CONST, NULL); }
Readonly <- "readonly" { $$ = CT(READONLY, NULL); }

#
# Type
#

Types <- t:Type { $$ = CN(TYPES, 1, t); } (_ "," _ t:Type { AC($$, t); })*
Type <- (t:BasicType / t:ListType / t:AppType / t:TupleType / t:MapType / t:ConstructorType / t:TypeVariable) { $$ = t; }
BasicType <- (b:BoolType / b:IntType / b:FloatType / b:StringType / b:JobType / b:ChannelType) { $$ = b;}
BoolType <- "Bool" { $$ = CT(BOOL_TYPE, NULL); }
IntType <- "Int" { $$ = CT(INT_TYPE, NULL); }
FloatType <- "Float" { $$ = CT(FLOAT_TYPE, NULL); }
StringType <- "String" { $$ = CT(STRING_TYPE, NULL); }
JobType <- "Job" { $$ = CT(JOB_TYPE, NULL); }
ChannelType <- "Channel" { $$ = CT(CHANNEL_TYPE, NULL); }
ListType <- "[" _ t:Type _ "]" {$$ = CN(LIST_TYPE, 1, t); }
AppType <- "(" _ "[" _ a:ArgTypes? _ "]" _ "->" _ r:Type _ ")" { $$ = CN(APP_TYPE, 2, a, r); }
ArgTypes <- a:Type { $$ = CN(ARG_TYPES, 1, a); } (_ "," _ a:Type { AC($$, a); })*
TupleType <- "(" _ t:Type { $$ = CN(TUPLE_TYPE, 1, t); } (_ "," _ t:Type { AC($$, t); })* _ ")"
MapType <- "[" _ k:Type _ ":" _ v:Type _ "]" { $$ = CN(MAP_TYPE, 2, k, v); }
ConstructorType <- n:Name ("<" _ t:Types _ ">")? { $$ = CN(CONSTRUCTOR_TYPE, 2, n, t); }
TypeVariables <- t:TypeVariable {$$ = CN(TYPE_VARIABLES, 1, t); } (_ "," _ t:TypeVariable { AC($$, t); })*
TypeVariable <- "$" Identifier { $$ = CT(TYPE_VARIABLE, $0); }

#
# Expression
#

Expr <- e:BindExpr { $$ = e; }
BindExpr <- l:MatchExpr _ ":=" _ r:Expr { $$ = CN(BIND, 2, l, r); } / e:LogicalOrExpr { $$ = e; }
LogicalOrExpr <- l:LogicalOrExpr _ "||" _ r:LogicalAndExpr { $$ = CN(OR, 2, l, r); } / e:LogicalAndExpr { $$ = e; }
LogicalAndExpr <- l:LogicalAndExpr _ "&&" _ r:BitwiseAndExpr { $$ = CN(AND, 2, l, r); } / e:BitwiseAndExpr { $$ = e; }
BitwiseAndExpr <- l:BitwiseAndExpr _ "&" _ r:BitwiseOrExpr { $$ = CN(BITWISE_AND, 2, l, r); } / e:BitwiseOrExpr { $$ = e; }
BitwiseOrExpr <- l:BitwiseOrExpr _ "|" _ r:GTEIntExpr { $$ = CN(BITWISE_OR, 2, l, r); } / e:GTEIntExpr { $$ = e; }
GTEIntExpr <- l:GTEIntExpr __ "gte" __ r:GTEFloatExpr { $$ = CN(GTE_INT, 2, l, r); } / e:GTEFloatExpr { $$ = e; }
GTEFloatExpr <- l:GTEFloatExpr __ "fgte" __ r:GTIntExpr { $$ = CN(GTE_FLOAT, 2, l, r); } / e:GTIntExpr { $$ = e; }
GTIntExpr <- l:GTIntExpr __ "gt" __ r:GTFloatExpr { $$ = CN(GT_INT, 2, l, r); } / e:GTFloatExpr { $$ = e; }
GTFloatExpr <- l:GTFloatExpr __ "fgt" __ r:LTEIntExpr { $$ = CN(GT_FLOAT, 2, l, r); } / e:LTEIntExpr { $$ = e; }
LTEIntExpr <- l:LTEIntExpr __ "lte" __ r:LTEFloatExpr { $$ = CN(LTE_INT, 2, l, r); } / e:LTEFloatExpr { $$ = e; }
LTEFloatExpr <- l:LTEFloatExpr __ "flte" __ r:LTIntExpr { $$ = CN(LTE_FLOAT, 2, l, r); } / e:LTIntExpr { $$ = e; }
LTIntExpr <- l:LTIntExpr __ "lt" __ r:LTFloatExpr { $$ = CN(LT_INT, 2, l, r); } / e:LTFloatExpr { $$ = e; }
LTFloatExpr <- l:LTFloatExpr __ "flt" __ r:NotEqualExpr { $$ = CN(LT_FLOAT, 2, l, r); } / e:NotEqualExpr { $$ = e; }
NotEqualExpr <- l:NotEqualExpr _ "!" t:Name "=" _ r:EqualExpr { $$ = CN(NE, 3, l, t, r); } / e:EqualExpr { $$ = e; }
EqualExpr <- l:EqualExpr _ "=" t:Name "=" _ r:InExpr { $$ = CN(EQ, 3, l, RN(t, EQ_TYPE), r); } / e:InExpr { $$ = e; }
InExpr <- l:InExpr _ "in" _ e:RightShiftExpr { $$ = CN(IN, 2, l, e); } / e:RightShiftExpr { $$ = e; }
RightShiftExpr <- l:RightShiftExpr __ "bsr" __ e:LeftShiftExpr { $$ = CN(BSR, 2, l, e); } / e:LeftShiftExpr { $$ = e; }
LeftShiftExpr <- l:LeftShiftExpr __ "bsl" __ e:ConsExpr { $$ = CN(BSL, 2, l, e); } / e:ConsExpr { $$ = e; }
ConsExpr <- l:ConsExpr _ "::" _ r:ListConcatExpr { $$ = CN(CONS, 2, l, r); } / e:ListConcatExpr { $$ = e; }
ListConcatExpr <- l:ListConcatExpr _ "@" _ r:MapConcatExpr { $$ = CN(CONCAT_LIST, 2, l, r); } / e:MapConcatExpr { $$ = e; }
MapConcatExpr <- l:MapConcatExpr _ "~" _ r:StringConcatExpr { $$ = CN(CONCAT_MAP, 2, l, r); } / e:StringConcatExpr { $$ = e; }
StringConcatExpr <- l:StringConcatExpr _ "^" _ r:MinusIntExpr { $$ = CN(CONCAT_STRING, 2, l, r); } / e:MinusIntExpr { $$ = e; }
MinusIntExpr <- l:MinusIntExpr _ "-" _ r:MinusFloatExpr { $$ = CN(MINUS_INT, 2, l, r); } / e:MinusFloatExpr { $$ = e; }
MinusFloatExpr <- l:MinusFloatExpr _ "-." _ r:PlusIntExpr { $$ = CN(MINUS_FLOAT, 2, l, r); } / e:PlusIntExpr { $$ = e; }
PlusIntExpr <- l:PlusIntExpr _ "+" _ r:PlusFloatExpr { $$ = CN(PLUS_INT, 2, l, r); } / e:PlusFloatExpr { $$ = e; }
PlusFloatExpr <- l:PlusFloatExpr _ "+." _ r:ModulusExpr { $$ = CN(PLUS_FLOAT, 2, l, r); } / e:ModulusExpr { $$ = e; }
ModulusExpr <- l:ModulusExpr _ "%" _ r:DivideIntExpr { $$ = CN(MODULUS, 2, l, r); } / e:DivideIntExpr { $$ = e; }
DivideIntExpr <- l:DivideIntExpr _ "/" _ r:DivideFloatExpr { $$ = CN(DIVIDE_INT, 2, l, r); } / e:DivideFloatExpr { $$ = e; }
DivideFloatExpr <- l:DivideFloatExpr _ "/." _ r:MultiplyIntExpr { $$ = CN(DIVIDE_FLOAT, 2, l, r); } / e:MultiplyIntExpr { $$ = e; }
MultiplyIntExpr <- l:MultiplyIntExpr _ "*" _ r:MultiplyFloatExpr { $$ = CN(MULTIPLY_INT, 2, l, r); } / e:MultiplyFloatExpr { $$ = e; }
MultiplyFloatExpr <- l:MultiplyFloatExpr _ "*." _ r:ExponentiationExpr { $$ = CN(MULTIPLY_FLOAT, 2, l, r); } / e:ExponentiationExpr { $$ = e; }
ExponentiationExpr <- l:ExponentiationExpr _ "^^" _ r:NotExpr { $$ = CN(EXPONENTIATE, 2, l, r); } / e:NotExpr { $$ = e; }
NotExpr <- "!" _ l:UnaryPlusIntExpr { $$ = CN(NOT, 1, l); } / e:UnaryPlusIntExpr { $$ = e; }
UnaryPlusIntExpr <- "+" _ l:UnaryPlusFloatExpr { $$ = CN(UNARY_PLUS_INT, 1, l); } / e:UnaryPlusFloatExpr { $$ = e; }
UnaryPlusFloatExpr <- "+." _ l:UnaryMinusIntExpr { $$ = CN(UNARY_PLUS_FLOAT, 1, l); } / e:UnaryMinusIntExpr { $$ = e; }
UnaryMinusIntExpr <- "-" _ l:UnaryMinusFloatExpr { $$ = CN(UNARY_MINUS_INT, 1, l); } / e:UnaryMinusFloatExpr { $$ = e; }
UnaryMinusFloatExpr <- "-." _ l:PostfixExpr { $$ = CN(UNARY_PLUS_FLOAT, 1, l); } / e:PostfixExpr { $$ = e; }
PostfixExpr <- (p:PrimaryExpr { $$ = CN(POSTFIX_EXPR, 1, p); }
                (_ "." _ n:Name { AC($$, RN(n, DOT_NAME)); } /
                 _ "[" _ s:Expr _ ".." _ e:Expr _ "]" { AC($$, CN(LIST_SLICE, 2, s, e)); } /
                 _ "[" _ i:IndexValues _ "]" { AC($$, CN(LIST_UPDATE, 1, i)); } /
                 _ "[" _ m:MapKeyValues _ "]" { AC($$, CN(MAP_UPDATE, 1, m)); } /
                 _ "[" _ e:Expr _ "]" { AC($$, CN(LIST_LOOKUP, 1, e)); } /
                 _ ("<" _ e:Expr _ ">")? "(" _ a:Args? _ ")" { AC($$, CN(FUNCTION_CALL, 2, e, a)); })*)

IndexValues <- i:IndexValue { $$ = CN(INDEX_VALUES, 1, i); } (_ "," _ i:IndexValue { AC($$, i); })*
IndexValue <- i:Int _ "=" _ v:Expr { $$ = CN(INDEX_VALUE, 2, i, v); }

PrimaryExpr <- "nil" { $$ = CT(NIL, NULL); } /
               "this" { $$ = CT(THIS, NULL); } /
               "self" { $$ = CT(SELF, NULL); } /
               "$" { $$ = CT(SLICE_LENGTH, NULL); } /
               e:ControlFlowExpr { $$ = e; } /
               e:Literal { $$ = e; } /
               e:Name { $$ = e; } /
               "(" _ e:Expr _ ")" { $$ = e; }

ControlFlowExpr <- (c:IfExpr / c:CaseExpr / c:ReceiveExpr / c:BlockExpr) { $$ = c; }

IfExpr <- "if" __ ie:Expr _ b:BlockExpr { $$ = CN(IF_EXPR, 1, CN(IF, 2, ie, b)); }
          (_ "elif" __ ee:Expr _ b:BlockExpr { AC($$, CN(ELIF, 2, ee, b)); })*
          (_ "else" _ e:BlockExpr { AC($$, CN(ELSE, 1, e)); })

CaseExpr <- "case" __ e:Expr { $$ = CN(CASE_EXPR, 1, CN(CASE, 1, e)); } _ "{"
              (_ m:MatchExprs _ ("when" _ we:Expr { AC($$, CN(WHEN, 1, we)); } _)? _ b:BlockExpr { AC($$, CN(CASE_BRANCH, 2, m, b)); })+
              (_ "default" _ b:BlockExpr { AC($$, CN(DEFAULT, 1, b)); })? _
              "}"

ReceiveExpr <- "receive" __ c:ReceiveChannels { $$ = CN(RECEIVE_EXPR, 1, CN(RECEIVE, 1, c)); } (_ "{"
               (_ m:MatchExprs _ b:BlockExpr _ { AC($$, CN(CASE, 2, m, b)); })+
               (_ "timeout" _ d:DecimalInt _ b:BlockExpr { AC($$, CN(TIMEOUT, 2, d, b)); })? _
               "}")?

ReceiveChannels <- (c:Channel / "[" _ c:Channels _ "]") { $$ = c; }
Channel <- (c:ChannelName { $$ = CN(CHANNEL, 1, c); }
                (_ "." _ n:Name { AC($$, RN(n, DOT_NAME)); } /
                 _ "[" _ e:Expr _ "]" { AC($$, CN(LIST_LOOKUP, 1, e)); })*)
ChannelName <- Identifier { $$ = CT(CHANNEL_NAME, $0); }
Channels <- c:Channel { $$ = CN(CHANNELS, 1, c); } (_ "," _ c:Channel { AC($$, c); })*

Literal <- (l:BasicLiteral / l:CompositeLiteral) { $$ = l; }

BasicLiteral <- (b:BoolLiteral / b:NumberLiteral / b:CharLiteral /
                 b:StringLiteral / b:FunctionLiteral / b:EnumLiteral) { $$ = b; }

CompositeLiteral <- (c:TupleLiteral / c:ListLiteral / c:MapLiteral / c:TypeConstructorLiteral) { $$ = c; }

BoolLiteral <- "true" { $$ = CT(TRUE, NULL); } / "false" { $$ = CT(FALSE, NULL); }

NumberLiteral <- (n:Float / n:Int) { $$ = n; }

Float <- (f:DecimalPointLeading / f:DecimalPointTrailing) { $$ = CT(FLOAT, $0); }
DecimalPointLeading <- [0-9]* "." [0-9]+ ExponentPart?
DecimalPointTrailing <- [0-9]+ ExponentPart
ExponentPart <- [eE] [+-]? [0-9]+

Int <- (HexInt / BinaryInt / OctalInt / DecimalInt) { $$ = CT(INT, $0); }
HexInt <- "0x" [0-9a-fA-F]+
BinaryInt <- "0b" [01]+
OctalInt <- "0" [0-7]+
DecimalInt <- [0-9]+

CharLiteral <- "'" (c:EscapeChar / c:NonQuoteChar) "'" { $$ = CN(CHARACTER_LITERAL, 1, c); }
EscapeChar <- "\\" (
              [abfnrtv'"\\] /
              "x" HexDigit HexDigit /
              "u" HexDigit HexDigit HexDigit HexDigit /
              "U" HexDigit HexDigit HexDigit HexDigit
                  HexDigit HexDigit HexDigit HexDigit /
               OctalDigit /
               OctalDigit OctalDigit /
               OctalDigit OctalDigit OctalDigit
               ) { $$ = CT(ESCAPE_CHARACTER, $0); }
HexDigit <- [0-9a-fA-F]
OctalDigit <- [0-7]
NonQuoteChar <- [^'] { $$ = CT(NON_QUOTE_CHARACTER, $0); }

StringLiteral <- ('"' s:RegularString '"' / 'r"' s:RawString '"') { $$ = s; }
RegularString <- (EscapeSequence / [^"])* { $$ = CT(REGULAR_STRING, $0); }
EscapeSequence <- "\\" [btnvfr"\\]
RawString <- [^"]* { $$ = CT(RAW_STRING, $0); }

FunctionLiteral <- "fn" _ "(" _ p:Params? _ ")" _ e:BlockExpr { $$ = CN(FUNCTION_LITERAL, 2, p, e); }

EnumLiteral <- edn:EnumDefName _ "#" _ en:EnumName { $$ = CN(ENUM_LITERAL, 2, edn, en); }

TupleLiteral <- "(" _ ")" { $$ = CT(TUPLE_LITERAL, NULL); } / "(" _ e:TupleExprs _ ")" { $$ = RN(e, TUPLE_LITERAL); }
TupleExprs <- e1:Expr _ "," _ e2:Expr { $$ = CN(EXPRS, 2, e1, e2); } (_ "," _ e:Expr { AC($$, e); })*

ListLiteral <- "[" _ "]" { $$ = CT(LIST_LITERAL, NULL); } / "[" _ e:Exprs? _ "]" { $$ = RN(e, LIST_LITERAL); }
Exprs <- e:Expr { $$ = CN(EXPRS, 1, e); } (_ "," _ e:Expr { AC($$, e); })*

MapLiteral <- "[:]" { $$ = CT(MAP_LITERAL, NULL); } / "[" _ k:MapKeyValues? _ "]" { $$ = RN(k, MAP_LITERAL); }
MapKeyValues <- m:MapKeyValue { $$ = CN(MAP_KEY_VALUES, 1, m); } (_ "," _ m:MapKeyValue { AC($$, m); })*
MapKeyValue <- (k:Literal / k:Name) _ ":" _ v:Expr { $$ = CN(MAP_KEY_VALUE, 2, k, v); }

TypeConstructorLiteral <- n:Name "<" _ e:Exprs _ ">" { $$ = CN(TYPE_CONSTRUCTOR_LITERAL, 2, n, e); }

#
# Match expression
#

MatchExpr <- (m:MatchCons / m:MatchLiteral / (m:UnboundName (_ "is" _ t:Type { AC(m, t); })?) / m:Name) { $$ = m; } (_ "as" _ u:UnboundName { AC($$, RN(u, MATCH_AS)); })?

MatchCons <- l:MatchExpr _ "::" _ r:MatchExpr { $$ = CN(CONS, 2, l, r); }

MatchLiteral <- (m:MatchBasicLiteral / m:MatchCompositeLiteral) { $$ = m; }

MatchBasicLiteral <- (m:BoolLiteral /
                      m:NumberLiteral /
                      m:CharLiteral /
                      m:StringLiteral /
                      m:EnumLiteral) { $$ = m; }

MatchCompositeLiteral <- (m:MatchTupleLiteral /
                          m:MatchListLiteral /
                          m:MatchMapLiteral /
                          m:MatchClassLiteral /
                          m:MatchTypeConstructorLiteral) { $$ = m; }

MatchTupleLiteral <- "(" _ ")" { $$ = CT(TUPLE_LITERAL, NULL); } / "(" _ m:MatchTupleExprs _ ")" { $$ = RN(m, TUPLE_LITERAL); }
MatchTupleExprs <- m1:MatchExpr _ "," _ m2:MatchExpr { $$ = CN(MATCH_EXPRS, 2, m1, m2); } (_ "," _ m:MatchExpr { AC($$, m); })*

MatchListLiteral <- "[" _ "]" { $$ = CT(LIST_LITERAL, NULL); } /
                    n:Name "[" _ m:MatchExpr? _ "]" { $$ = CN(LIST_LOOKUP, 2, n, m); } /
                    "[" _ m:MatchExprs? _ "]" { $$ = RN(m, LIST_LITERAL); }
MatchExprs <- m:MatchExpr { $$ = CN(MATCH_EXPRS, 1, m); } (_ "," _ m:MatchExpr { AC($$, m); })*

MatchMapLiteral <- "[:]" { $$ = CT(MAP_LITERAL, NULL); } / "[" _ m:MatchMapKeyValues? _ "]" { $$ = RN(m, MAP_LITERAL); }
MatchMapKeyValues <- m:MatchMapKeyValue { $$ = CN(MAP_KEY_VALUES, 1, m); } (_ "," _ m:MatchMapKeyValue { AC($$, m); })*
MatchMapKeyValue <- (k:Literal / k:Name) _ ":" _ v:MatchExpr { $$ = CN(MAP_KEY_VALUE, 2, k, v); }

MatchClassLiteral <- n:Name _ "(" _ m:NamedMatchArgs _ ")" { $$ = CN(MATCH_CLASS_LITERAL, 2, n, m); }
NamedMatchArgs <- n:NamedMatchArg { $$ = CN(NAMED_MATCH_ARGS, 1, n); } (_ "," _ n:NamedMatchArg { AC($$, n); })*
NamedMatchArg <- u:UnboundName { $$ = CN(NAMED_MATCH_ARG, 1, u); } / n:Name _ ":" _ r:MatchExpr { $$ = CN(NAMED_MATCH_ARG, 2, n, r); }

MatchTypeConstructorLiteral <- n:Name "<" _ m:MatchExprs _ ">" { $$ = CN(MATCH_TYPE_CONSTRUCTOR_LITERAL, 2, n, m); }

#
# Misc
#

Identifier <- [a-zA-Z_][a-zA-Z_0-9_]*
Name <- Identifier { $$ = CT(NAME, $0); }
UnboundName <- "?" n:Name { $$ = RN(n, UNBOUND_NAME); } / "_" { $$ = CT(UNBOUND_NO_NAME, $0); }
#_ <- WS*
#__ <- WS+
_ <- (WS / Comments)*
__ <- (WS / Comments)+
WS <- [ \t\r\n] {
    if ($0[0] == '\n') {
        auxil->row++;
    }
}
Comments <- SingleLineComment / BlockComment
SingleLineComment <- "//" (!EOL .)* EOL?
EOL <- ("\r\n" / "\n" / "\r") { auxil->row++; }
BlockComment <- "/*" (BlockCommentContent / EOL)* "*/"
BlockCommentContent <- (!("*/" / EOL) .)
EOF <- _ !.

%%
int main() {
#ifdef UNITTEST
    symbol_table_unit_test();
#endif
    satie_auxil_t* satie_auxil = satie_auxil_new();
    satie_context_t *context = satie_create(satie_auxil);
    ast_node_t* program;
    satie_parse(context, &program);
    //hm_infer_types(program);
    ast_print(program, 0);
    satie_destroy(context);
    return 0;
}
```

# Appendix C: A todo list example

```
import Std.stdio : writeln
import Std.lists

class TodoItem {
    private description
    private completed

    this(description) {
        this(descriptor: descriptor, completed: false)
    }

    public fn toString() {
       if completed {
           "[x] "
       } else {
           "[ ] "
       }
    }
}

class TodoList {
    private items = [:]

    public fn addItem(tag, description) {
        ?item := TodoItem(description),
        this(items: [tag: item] ~ items)
    }

    public fn markItemCompleted(tag) {
        ?item := items[tag].setCompleted(true),
        this(items: item ~ items.delete(tag))
    }

    public fn displayItems() {
        lists.foreach(fn () { writeln(item.toString())}, items)
    }
}

export fn main() {
    fn loopUntilQuit(todoList) {
        ?input := readInput(), // implemented elsewhere
        if input.command =String= "add" {
            (?todoList, ?a) := todoList.addItem(input.tag, input.description),
            loopUntilQuit(todoList)
        } elif input.command =String= "complete" {
            ?todoList := todoList.markItemCompleted(input.tag),
            loopUntilQuit(todoList)
        } elif input.command =String= "show" {
            todoList.displayItems(),
            loopUntilQuit(todoList)
        } elif input.command =String= "quit" {
            true
        } else {
          stdio.writeln("Unknown command: $input.command"),
          loopUntilQuit(todoList)
        }
    },
    ?todoList := TodoList(),
    loopUntilQuit(todoList)
}
```
*Source: [todo.sa](grammar/examples/todo.sa)*
