# The R programming language

Everything is an expression.

Nothing can be declared in the global context except for the mandatory
main function (which must be declared in the global context in one module).

```
fn main(args) {
  b
}
```

> [!NOTE]
> No pointers but everything is refered to by reference. It's true. :-)

> [!NOTE]
> Dynamically typed to start with but the goal is to add type
> inference and a gradual type system.

> [!NOTE]
> Everythng is UTF8

## Comments

`//` and `/* ... */`

## Variables

Valid variable characters: `_?a-zA-Z[0-9|a-zA-Z]*`

```
a = 1
a3 = "foo"
```

## Enums

```
enums Bonk {
  a
  b
  c
}
```

Used as

```
Bonk:c
Bonk:a
```

## Numbers

`3.0` is a float
`3` is an int

`3.0 + 3` is not allowed

`int!3.0 + 3` is allowed (casting)

and

`3.0 + float!3`

or when using variables

```
a = 3.0
b = 3
c = int!a + b
d = a + (float!b - 1.0)
```

## Booleans

```
a = true
b = false
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

All elements in an array must have the same type

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
e = 4711 ~> b;          // e = [4711, 2, 23] (COPY)
f = b <~ 4711;          // e = [2, 23, 4711]
g = f.dup();            // Explicit copy
```

> [!NOTE] Implemented internally with a double-ended queue (dynamic
> array of continous memory).

## Hash maps

All keys and values may have any type
```
a = [ "a" : 1.0, "b" : "foo" ]
a["a"] = "bar"
a[42] = "baz"           // a = ["a" : "bar", "b" : "foo", 42 : "baz"]
b = a                   // b = ["a" : "bar", "b" : 0, 42 : "baz"]
b["a"] = 0              // a = ["a" : 0, "b" : 0, 42 : "baz"]
                        // b = ["a" : 0, "b" : 0, 42 : "baz"]
c = a["a"]              // c = 0
d = b.dup()             // Explicit copy
```

> [!NOTE]
> Note: If the key is non-primitive, e.g. a tuple or an object, the
> key is first serialized. Costly but i think it's for the best. May
> change my mind.

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
    true <- map(l, f)
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

The `<-` is used for matching. More on that below.

Calling convention:

`foo(a, b)`

> [!NOTE]
> No support for currying and variadic parameters

## Records

```
record Foo {
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

A record Foo is instantiated like this:

`foo = new Foo(1, 2)`

Access to member variables and functions:

```
foo.a
foo.a = 1
foo.bar(1)
```

Records can implement  pre-defined interafaces as well and an
interface is defined like this:

```
interface Bar {
  a
  public fn foo()
}

And can used like this:

record Foo : Bar {
   // See Foo record below
}
```

## Matching

```
match expr {
  match-expr =>
    a
    b
  match-expr =>
    c
}
```

such as

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

> [!NOTE]
> `?` introduces a fresh variable and `_` is a wildcard



## Hierachical modules

```
import std.stdio
stdio.writeln("foo")
```

```
import std.stdio : writeln writefln
writeln("foo")
```

```
import std.stdio : *
writeln("foo")
```





Name conflicts are checked during usage of module

inga macron

Singleton


a(l, 10)





fn a(l, m, n = 0) {
    if (n > m) {
        l
    } else {
        ln[n] = l[n] + 1
        a(l, m, n + 1)
    }
}


fn a(l, m, fun, n = 0) {
    if (n > m) {
        l
    } else {
         ln[n] = fun(l, n)
         a(l, m, n + 1)
    }
}

l.dup().map(l, fun)



fn main() {
    l = [1, 2, 3]
    f = (l, n) => { l[n] + 1 }
    true <- map(l, f)
}

fn map(l, f, n = 0) {
    if (n > l.length()) {
        true
    } else {
         l[n] = f(l, n)
         a(l, f, n + 1)
    }
}







    a(l[n] , n) ->



  a(l[1 .. $]) <~ l[0] + 1



  a(b[1 .. $ - 1] <~ a[0])
