# The R programming language

Everything is an expression and Unicode is everywhere.

Nothing can be declared in the global context except for the mandatory
main function which **must** be declared in the global context (by one
of the modules constituting an application).

```
fn main(args) {
  0
}
```

> [!NOTE]
> R contains no pointers and everything except `int`, `float` and
> `bool` values are referred to by reference.

> [!NOTE]
> R is dynamically typed to start with but the goal is to add type
> inference and gradual typing.

Reserved words: `if`, `then`, `else`, `match`, `=`, `enum`, `:`,
`true`, `false`, `#`, `+`, `-`, `*`, `/`, `fn`, `[`, `]`, `{`, `}`,
`"`, `(`, `)`, `$`, `~`, `'`, `<@`, `@>`, `<~`, `class`, `public`,
`private`, `readonly`, `const`, `this`, `new`, `interface`,
`singleton`, `.`, `import`
`=>`, `?` and all other binary and unary operators you can think of.

## Comments

`//` and `/* ... */`

## Literals

`_?a-zA-Z[0-9|a-zA-Z]*`

## Characters

`'B' and '\n'` etc

## Variables

```
aVariable = 1
B52 = "foo"
```

## Enums

```
enum Bonk {
  a
  b
  c
}
```

Refer to them like this:

```
Bonk:c
Bonk:a
```

## Numbers

`3.0` is a float
`3` is an int

`3.0 + 3` is not allowed

`int#3.0 + 3` is allowed (# is the casting operator)

and

`3.0 + float#3`

and with variables:

```
a = 3.0
b = 3,
c = int#a + b
d = a + (float#b - 1.0)
```

## Booleans

```
true
false
```

## Strings

Immutable

`"foo"`

`"foo $a is not ${a + 1.0}"` becomes `"foo 3.0 is not 4.0"`

```
a = "foo"
b = "bar"
c = a ~ b               // c = "foobar" (COPY)
```

## Tuples

```
'(1, 2)
'(a, '(b, 4))
```

## Dynamic arrays

All elements in an array must have the same type:

```
a = [1, 2, 3, 4, 5];
b = a[1 .. 3];          // b = [2, 3]
c = a[2 .. $ - 1];      // c = [3, 4]
d = b ~ c;              // d = [2, 3, 3, 4] (COPY)
d[1] = 42;              // d = [2, 42, 3, 4]
a[2] = 23;              // a = [1, 2, 23, 4, 5]
                        // b = [2, 23]
                        // c = [23, 4]
                        // d = [2, 42, 3, 4]
e = b.dup()             // Explicit copy
4711 @> b;              // b = [4711, 2, 23]
e <@ 4711;              // e = [2, 23, 4711]
```

> [!NOTE]
> Internally implemented with a double-ended queue (dynamic
> array of continous memory).

## Hash maps

All keys and values may have any type:

```
a = [ "a" : 1.0, "b" : "foo" ]
a["a"] = "bar"
a[42] = "baz"           // a = ["a" : "bar", "b" : "foo", 42 : "baz"]
b = a                   // b = ["a" : "bar", "b" : 0, 42 : "baz"]
b["a"] = 0              // a = ["a" : 0, "b" : 0, 42 : "baz"]
                        // b = ["a" : 0, "b" : 0, 42 : "baz"]
c = a["a"]              // c = 0
d = a ~ [42 : 4711]     // d = [42 : 4711, "a" : 0, "b" : 0] (COPY)
e = b.dup()             // Explicit copy
```

> [!NOTE]
> Structural equality is used for key values

## Classes

```
class Foo {
  public a
  private b
  readonly c
  const e

  this(a, g) {  // Constructor
    this.a = a;
    b = g;
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
> No support for inheritence (see interface below)

A class Foo can be instantiated like this:

`foo = new Foo(1, 2)`

Access to member variables and functions look like this:

```
foo.a
foo.a = 1
foo.bar(1)
```

A class may opt to implement a certain interface. The interface
defines which member variables and functions that should be mandatory
in a class. An interface can look like this:

```
interface Bar {
  public a
  public fn foo()
}
```

A class which decides to implement this interface can look like this:

```
class Foo : Bar {
   // See Foo class above
}
```

A class can also be defined as a singleton:

```
singleton class Foo : Bar {
   // See Foo record above
}
```

It means what you think.

## Control statements

```
if expr {
  a
} else {
  b
  c
}
```

## Functions

Define a named function like this:

```
fn foo(a, b, c = 0) {
  c
  d
}
```

Trailing parameters may have default values.

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
(a, b) => {
    b
}
```

This is an example of a map function:

```
fn main() {
    l = [1, 2, 3]
    f = (l, n) => { l[n] + 1 }
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

Calling convention:

`foo(a, b)`

> [!NOTE]
> No support for currying and variadic parameters

## Matching

Matching can be done with the `<~` operator:

```
a = 1
'(a, ?a, 1) <~ '(1, 2, 1)     // a = 2
'(?a, b, ?h) <~ foo(42)
```

`?` introduces unbound variables.

Matching can also be done this way:

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

An example:

```
a = 1
b = 3
match expr {
  '(1, ?a) {
    a
  }
  a | b {
    a + 1
  }
  _ {
    0
  }
}
```

`_` is a wildcard.

## Macros

No macros

## Hierarchical modules

```
import std.stdio
stdio.writeln("foo")
```

```
import std.stdio : writeln, writefln
writeln("foo")
```

```
import std.stdio : *
writeln("foo")
```

Name conflicts are checked when something is referred to in imported
modules.

The `std.jobs` module contains the functionality needed to work with
concurrent jobs and message passing in between them, i.e. `spawn()`,
`send()` and `recv()` functions.
