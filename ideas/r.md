# The Satie Programming Language

## Introduction

Satie is envisioned as a forward-thinking programming language, ideal
for crafting programming editors of tomorrow. Its capabilities
extend beyond merely constructing the editor; it is also adept at
serving as the scripting language for creating editor plugins and
customizations. Yet, the essence of Satie lies in its versatility — it
is a purpose-built language and Virtual Machine (VM) that boast a
high degree of generality, adaptable to a wide range of applications.

Satie owes much to the great people behind the
[Erlang](https://www.erlang.org/) and [D](https://dlang.org/)
programming languages (and all people standing behind [and beside] them).

All rise. A tribute and crash course:

```
$ cat tribute.sa
import std.stdio : writeln
import std.lists

export fn main(args) {
    ?numberOfTributes = args[1],
    ?jobs = startTributes(numberOfTributes),
    lists.foreach(fn (job) {
        job <| "Standing on the shoulders of giants"
    }, jobs)
}

fn startTributes(m, n = m, jobs = []) {
    if n < m {
        ?job = spawn fn () {
            receive {
                case ?message {
                   writeln("$n: $message")
                }
            }
        },
        startTributes(m, n + 1, job ~ jobs)
    } else {
        jobs
    }
}
$ sac tribute.sa && sa build/hello 100000
0: Standing on the shoulders of giants
1: Standing on the shoulders of giants
2: Standing on the shoulders of giants
3: Standing on the shoulders of giants
...
99999: Standing on the shoulders of giants
```
*Source: [tribute.sa](../grammar/tribute.sa)*

That said.

The following design choices have been made (in some sort of order):

### Concurrent Oriented

 * Satie is built on a custom built multi-core VM with strong support
   for  time sliced green threads (from now on called *jobs*). Jobs
   have share nothing semantics relying solely on message passing to
   make it easier to reason about, and implement highly concurrent,
   massively scalable soft real-time systems with an emphasis on fault
   tolerance and high availability. Jobs can create monitors and links
   between each other and this makes it possible to write supervisor
   jobs that are responsible to restart jobs if they should die
   unexpectedly.

### Functional and Immutable

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
   `struct`). The compiler uses Hindley–Milner type inference to deduce
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
   on that. Satie reserves 25 keywords and sports few operators and the
   complete syntax is formally defined as a PEG grammar in appendix B.

 * Satie has pattern matching in its core and the `=` operator is
   actually all about pattern matching rather than assignment (*there
   must not be mutable updates*). Everything can be matched and taken
   apart with the help of a pattern matching in combination with the
   `=` operator. Pattern matching is also used by the `switch`
   expression and the `receive` expression also uses pattern matching to do
   selective receive on messages in a job's mailbox.

 * Satie is implemented using a custom built VM consisting of a
   multi-core and time slicing job scheduler running multiple
   instances (one for each job) of a custom built register
   machine. The VM and its scheduler has a small memory footprint and
   each job startedinitially only allocates 1KB for its heap and stack
   (program code excluded). The VM is standalone and has few
   dependencies making it easy to port to restricted targets.

 * Great care has been taken to add a purely functional and
   encapsulating `struct` definition. It makes it possible group member
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

## Overall Structure

A Satie file has a `.sa` suffix and constitute a Satie *module* with
the same name as the basename of its filename. Why make it more
complicated? A satie file starts with a number of import statements
which are followed by a mix of `enum`, `interface`, `struct` and `fn`
(function) definitions (in any order).

`struct`, `enum`  and `interface` definitions are **only** allowed on
the top level of a module. Function definitions can be nested
arbitrarily within other function definitions though.

A single exported main function must be defined in exactly one of the
Satie modules that constitutes an application.

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

struct ColorIterator : Iterator {
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
member functions in structes to avoid function cluttering. Only
functions being marked with `export` can be imported by other
modules. The `struct`, `enum` and `interface` definitions can be
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

## Building and Executing

Satie's compiler is called `sac` and the byte code it produces can be
executed with the `sa` runner. This was done in the introduction chapter
above and is repeated here for clarity:

```
$ sac hello.sa
$ find .
.
./hello.sa
./build
./build/hello.sab
$ sa build/hello 100000
0: Standing on the shoulders of giants
1: Standing on the shoulders of giants
2: Standing on the shoulders of giants
3: Standing on the shoulders of giants
...
99999: Standing on the shoulders of giants
100000: Standing on the shoulders of giants
```

That is it.

If a Satie application consists of many modules in a directory
hierarchy the process above is the same. Say for example that there is
an application called *zarah* with the following directory hierachy:

```
$ cd zarah
$ find .
.
./src
./src/main.sa
./src/utils
./src/utils/database.sa
./src/utils/httpclient.sa
./src/utils/lists.sa

```

This is how zarah can be compiled and executed:

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
$ sa build/main
```

> [!NOTE]
> The `sac` compiler can be made to use alternative `src/` and
> `build/` directories

The compiler by default follows module dependencies introduced by
`main.sa` and automatically compiles those modules as well. The
compiler can be made not to follow module dependecies, ignore missing
modules, or modules not possible to compile. Read more about the `sac`
compiler and the `sa` runner in their respective manual page.

## The Shell

The `sa` runner can also start an interactive Satie shell:

```
$ sa --shell
Satie <0.3.1> (press ctrl-c to abort)
0> _
```

In the shell Satie expressions can be evaulated and the status of an
already executing application can be inspected, i.e a shell can be
made to connect to an already executing `sa` runner instance. Read
more about this and more in the `sa` runner's manual page.

# The Gory Details

## Comments

Everything after `//` and to end of line and within `/* ... */` are
considered comments.

## Types

### Basic Types

`bool` : A boolean value

`int` : A signed integer value. On a 64-bits machine integers are
handled naively if they fit within 61 bits, and if not they are
transparently represented as arbitrary-precision bignums. On a 32-bits
machine they must fit within 29 bits etc.

`float` :  A floating point value. On a 64-bits machine floats are
must fit fit within 61 bits. On a 32-bits machine they must within 29
bits etc.

`char` : A 32-bits Unicode code point value

`function` : A function reference

`job` : A job reference

`enum`: An enumeration value

### Composite Types

`string` : An immutable sequence of UTF-8 encoded characters

`tuple` : A fixed sized sequence of values of any type

`list`: A list of values of any type

`map`: A mapping between a key of any type and a value of any type

`struct` : A Unit of encapsulation for member variables and member
functions

`buf` : A buffer to efficiently manipulate large amount characters

### Type Management

All values can be type checked in run-time using the functions
`isBool`, `isInt`, `isFloat`, `isChar`, `isFunction`, `isJob`,
`isEnum`, `isString`, `isList`, `isMap`, `isStruct`, `isBuf` and
`typeof`.

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

```
?a = 3,
?b = 93326215443944152681,
?c = 3.0,
?d = b / cast(int)c + a,     // 31108738481314713603
d + c                        // Compiler error!
```

The `inspect` function provides even more run-time type information.

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

All values can be converted to string representation using the
`toString` function.

```
?a = 3.14,
?b = [Foo.a : 42, "bar": #(fn () { x + x}, [1, 2,3])],
a.toString(),       // "3.14"
b.toString()        // "[Foo.a : 42, "bar": fn/0]"
```

## Identifiers

An identifier is a case sensitive sequence of characters starting with
an ASCII letter or an underscore, followed by any number of ASCII
letters, underscores or digits,
i.e. `^[[:alpha:]_][[:alnum:]_]*$`. Keywords, variables, function
names, struct names and enum names are all identifiers.

> [!NOTE]
> By design only strings may contain Unicode characters. This
> restriction may be lifted if compelling reasons should appear.

### Keywords

The following 24 special identifiers are reserved by Satie and cannot
be used in user code:

```
import
true
false
enum
in
fn
export
if
else
switch
default
struct
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

## Literals

### Boolean Literals

`true` or `false`

### Integral Literals

An integral literal can be formatted as decimal, octal and hexadecimal
value.

```
4,             // Decimal format
017,           // Octal format
0xffff,        // Hexadecimal format
0b101010100    // Binary format
```

### Floating-point Literal

A floating point literal represent real number that include a
fractional part. It is written in a similar manner to other languages
like C or Java.

```
1.0,
.666e2
```

### Character Literal

A character literal is a Unicode code point value enclosed within
single quotation marks. It consists of four bytes under the hood.

```
'A',
'ω',
'\u03c9'    // ω
```

### Function Literal

A function literal follows the same syntax as regular function
definitions (see below) but with a function name.

```
?sum = fn (x, y) { x + y },
sum(1, 2)                   // 3
```

### Job Literals

Job literals are opaque.

### Enumeration Literal

An enumeration is a named constant and it is always defined in a named
[enumeration definition](enumeration). An enumeration literal is a
dot-separated enumeration name and constant name.

```
enum Color {
    red
    green
    blue
}

Color.red                   // An enumeration literal
```

### String Literal

A string literal is represented as an immutable UTF-8 encoded sequence
of Unicode characters enclosed within double quotation marks. Escape
sequences has meaning in double quoted strings. Raw strings are also
enclosed within double quotation marks but are prefixed with the
letter `r`. Escape sequences have no meaning in raw strings and all
characters are parsed verbatim.

```
?a = "fooω",
a[3],               // 'ω'
?b = r"foo\nbar"    // b.length == 8
```

### Tuple Literal

A tuple literal is represented as comma separated fixed size sequence
of values of any type enclosed between a leading `#(` and a trailing
`)`.

`#("foo", 3.14, #("bar", fn (x) { x + 1}))`

### List literal

A list literal is represented as comma-separated sequence of values of
any type enclosed within square brackets.

```
?a = [3.14, "foo", 1816381],
?b = a[1 = 42, 2 = "bar"]      // b == [3.14, 42, "bar"]
```

> [!NOTE]
> Only existing list entries can be updated this way

### Map literal

A map literal is represented as comma-separated sequence of key-values
of any type (separated by a `:` character) enclosed within square
brackets.

```
?a = ["foo" : 12, 3.14 : 981237198192378 ],
a[3.14: 4711, 2 : 4]                         // ["foo" : 12, 3.14: 4711, 2 : 4],
```

### Struct Literal

A struct literal is represented as a semicolon-separated sequence of
member-values, where the member is an identifier and the value is of
any type, (separated by a `=` character) enclosed in square brackets.

```
struct Foo P {
    public foo = 4711
    public bar = "foo"
    public zonk = #(42, 3.14)
}

?a = struct Foo(),
[foo ; ?b, bar ; ?c] = a,
b,                          // 4711
c                           // "foo"
```

## Expressions

Everything is an expression in Satie except for the top level
definitions, i.e. `import`, `struct`, `interface`, and `enum`.

Out of all expressions the *bind* expression, and its `=` operator,
stands out for its ability to bind variables to values. There is no
notion of variable assignment in Satie but instead unbound variables
are bound to values. An unbound variable starts with a `?` character
and a naked variable (without a `?` character) is required to be bind
before use.

```
?a = 42,
a = 42,
a = "foo"     // Compiler or runtime error!
```

In the above example `a = "foo"` leads to a compile time error at best
or at least a catastrophic runtime error, i.e. `a` is `42` not
`"foo"`. This kind of mismatch is most often used as an assertment or
it may be a software bug. A catastrophic runtime error stops the
execution of *job* in which the mismatch occured. Read more about
concurrency and jobs in the "Concurrency" chapter.

The following code snippet is rejected by the compiler if variable `a`
is unbound:

`a = 42`

The pattern matching performed by bind expressions can be useed to
deconstruct composite values into its primitive values:

```
fn foo(x) {
  #(4711, x + x, "bar")
},
?a = 1,
#(a, ?b, 1) = #(1, 2, 1),     // b == 2
#(?a, b, ?c) = foo(1),        // a == 4711 && c == "bar"
#(a, a, a) = c                // A catastrophic mismatch!
```

> [!NOTE]
> Above we used expressions not yet explained but it suffices to say
> that #(1, 2, 1) is a fixed size tuple.

Pretty nifty.

The `switch` and `receive` expressions also use pattern matching
able to deconstruct and select. Read more about them below.

Furthermore, the `=` operator is only allowed as a standalone
expression and cannot be used deep within expressions. For your
sanity's sake. Don't kill me. 😇

```
main() {
    ?a = 42,
    a + (?c = 42 + a) + a    // Compiler error!
}
```

### Control Flow Expressions

#### Block Expression -- `{` a, b, c, ... `}`

A block expression is a comma-separated sequence of expressions
enclosed in curly braces. Expressions are evaluated in a sequence and
introduces a lexical scope. An variable bound in a scope is visible to
all the following expressions in the scope. The variable is not
visible outside of the scope and it shadows an identifier with the
same name introduced outside of the scope. The value of the last
expression in the sequence is returned from the block.

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

#### If Expression -- `if`, `elif`, `else`

No surprises here.

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

#### Switch Expression -- `switch`, `case`, `default`

A switch expression uses pattern matching to dispatch between its case
paths but also to do deconstruction as introduced in the
[Expressions](#expressions) chapter.

In the first example things are kept simple:

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

In the second example a tuple is matched

```
?a = #("bar", 4711),
switch a {
    case "foo" {
        "Darn!"
    }
    case #("bar", ?c) {
        c
    }
    default {
        "No one picks me!"
    }
}
```

No more no less.

> [!NOTE]
> There is no fall through mechanism and the `default` keyword is
> optional. Switch fall through must be next most expensive mistake
> not counting Hoare's null pointer. YMMV.

### Value Expressions

#### Enumeration

Enumeration introduces named constans and can enhance code readability
and maintainability. An named constant can have an optional constant
value, of any type, attached to it.

```
enum Color {
    red = #(255, 0, 0)
    green = #(0, 255, 0)
    blue = #(0, 0, 255)
}

Foo.red,              // An enumeration literal
Foo.red.value         // #(255, 0, 0)
```

#### Function

`foo(a, b)` invokes the function `foo` with a comma separated list of
expression arguments. Arguments are evaluated left to right before the
function is invoked and `foo` may refer to a named function definition
or to a variable bound to a function literal.

Functions can be overloaded and function parameters have default values:

```
fn foo(a, b, c = 0) {
  c;
  d;
}

fn foo(a = 1) {
  a;
}
```

Default values can only be given to trailing parameters and these
parameters can be omitted in function calls. A function call can
either be called with positional parameters **only** or with named
parameters **only**. The following function calls are equivalent:

```
foo(2, 6),
foo(2, 6, 0),
foo(a: 2, b: 6),
foo(b: 6, a: 2)
```

Named functions can be defined within functions:

```
fn foo(a, b, c = 0) {
    fn bar(d) {
        d
    },
    bar(a)
}
```

Functions are first class citizens:

```
import stdio.lists

export fn main() {
    ?l = [1, 3, 2],
    ?f = fn (x, y) { x > y },
    lists.sort(l, f)
}
```

#### String

A string is an immutable sequence of UTF-8 encoded characters. String
interpolation is supported as well as random access to individual
charcters in a string even though the string is UTF-8 encoded.

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
#### Tuple -- `#(` a, b, c, ... `)`

A no-brainer.

```
?a = 42,
?b = #(4711, #(a, [1, 2])),
#(_, #(?a, [_, c])) = b,
a,                           // 4711
c                            // 2
```

#### List -- `[` a, b, c, ... `]`

A list contains elements of any type and list slicing makes it easy to
work with portions of a list. `a[i .. j]` returns a list slice
starting at index `i` and ending with index `j - 1`. `i` and `j` can
be any valid expression and the keyword `$` is the length of the list.

`a[i]` returns the i:th element.

`a[2 = "foo"]` evaluates to a new list with element 2 set to "foo".

```
?a = [],                 // An empty list
?a = [1, 2, 3, 4, 5],    // A rebind of variable a
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

#### Map  -- `[` a `:` b, ... `]`

A map can be seen as a function mapping between keys of any type and
values of any type.

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

#### Encapsulation -- `struct`

Member values and member functions and structs can
only be defined as top-level defintions in each module.

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

#### Buf(fer) -- `buf`

A buffer is an opaque persistent datatype which can be used to
efficiently manipulate large amount characters. It fits well in a
programming language intending make it easy to implement programming
editors (and more).

> [!NOTE]
> This part is very much under consideration nothing is set in stone

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
* Immutability: Each operation creates a new buf instance, preserving
the original and adhering to the principles of functional programming.
* Efficiency and Scalability: The design is optimized for common text
editing operations, with considerations for handling large files
through lazy loading.
* Clipboard Operations: The addition of select, cut, copy, and paste
functionalities provides essential editing capabilities.
* Extensibility: The design allows for future extensions, such as
language-specific features or plugins.
* Undo Mechanism: The undo functionality is natural and integral,
implemented via a history of states or operations.
* Concurrency Support (if applicable): Concurrency support is vital
for collaborative editing scenarios, requiring thread-safe operations
and potential conflict resolution strategies.
* Lazy Loading: This feature enables efficient handling of large
files, loading content as needed rather than all at once.

With these functionalities and considerations, the "buf" datatype
becomes a comprehensive and robust tool for building a programming
editor, offering a wide range of functionalities required for text
editing and manipulation in a functional programming environment.

# Hierarchy of Modules

A module is implemented in a file with a `.sa` suffix and the module
name is the basename of the filename. A module hiearchy is implemented
as a nested hierachy of directories.

Nothing new here but is all in the details.

A module hierarchy is a nice way to organize code and in the example
below a module uses the `foreach` and ` writeln` functions from the
standard libray:

```
import std.stdio
import std.lists

export fn main(args) {
  lists.foreach(fn (arg) { stdio.writeln("$arg") }, args)
}
```

The name of the modules in the standard library must be specified when
calling `foreach` and `writeln`, i.e. nothing is automatically
imported into the namespace of module itself.

It is possible to import functions and enumerations etc into the
namespace the module:

```
import std.stdio : writeln
import std.lists : foreach

export fn main(args) {
  foreach(fn (arg) { writeln("$arg") }, args)
}
```

In the [Building and Executing](building-and-executing) chapter the
*zarah* project was introduced and it had the follwoing module
hierarchy:

```
./src
./src/main.sa
./src/database/tablestore.sa
./src/database/backup.sa
./src/database/utils/lists.sa
./src/utils/httpclient.sa
```

The modules `std.lists` and `database.utils.lists` has the same module
name and to resolve this import aliasing is used:

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


### An example

Right in your face.

```
import std.jobs : OnCrowding, Job
import std.stdio
import std.lists

export fn main() {
  ?ackermann = new Ackermann(),
  ?ackermann = ackermann.startJobs(3, 10),
  ackermann.waitForJobs()
}

struct Ackermann {
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
| a[i]         | List indexing                            |
| a[b .. c]    | List slicing  (see "List Literal" above) |
| a[a = b]     | List setter (see "List Literal" above)   |
| a[a : b]     | Map setter (see "Map Literal" above)     |
| a[a ; b]     | Struct setter (see "Struct Literal")     |
| -a           |                                          |
| +a           |                                          |
| !a           |                                          |
| ~a           | Bitwise complement                       |
| <\|          | Send message                             |
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
| a == b       | Equality                                 |
| a &lt; b     |                                          |
| a &lt;= b    |                                          |
| a > b        |                                          |
| a >= b       |                                          |
| a \| b       |                                          |
| a ^ b        | Bitwise xor                              |
| a & b        |                                          |
| a && b       | Logical and                              |
| a \|\| b     |                                          |
| a = b        |                                          |

# Appendix B: PEG grammar

```
#
# Top level structure
#

Program <- _ (Imports __)? TopLevelDefs EOF
TopLevelDefs <- TopLevelDef (__ TopLevelDef)*
TopLevelDef <- StructDef / InterfaceDef / EnumDef / FunctionDef

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
           StructLiteral

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

StructLiteral <- "[" _ MemberValues? _ "]"
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
# Struct definition
#

StructDef <- "struct" __ Identifier _ ( ":" _ Interfaces _)?
                   "{" _ StructMembers _ "}"
Interfaces <- Identifier (_ "," _ Identifier)*
StructMembers <- StructMember (_ StructMember)*
StructMember <- Constructor / Deconstructor / MemberFunction / MemberVariable
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
