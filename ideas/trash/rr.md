## Functions

Define a named function like this:


a = 42


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

`foo(b = 6, a = 2)`

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

# The main function

Nothing can be declared in the global context except for the `main`
function which **must** be declared there.



class, main, enum
