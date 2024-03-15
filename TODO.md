# Releases

## Version 0.1

* [x] Dive into Hindley-Milner type inference
      _ Add equations for case + test case + as

      _ implement type and alias + type constructors (and _ type wildcard?)

      _ Print type error beautifully (Read reserahc paper again)

      _ Add column support in satie.peg
      _ check free malloc in hm.c etc + remove dtor?
      _ Parse import and do remote module type checking (load path)
        : to refer to module for all defs

      _ Skip enums, record and tasks for now


* [ ] Update `README.md` to a shining condition
* [ ] Update `SAI.md` to a shining condition
* [ ] Implement the instructions described in `SAI.md`
* [ ] Update `sac` compiler to generate register instructions
  - `sa -> ast -> sai -> sab`
  - Add exported type signaures to sab files
  - Use a dummy GC
  - Use copy-on-write instead of persistent datatypes

## Version 0.2

* GC
* Add values to enums
* Partial evauation of constant values
* Multi module compiling

## Version 0.3

* Design the buffer datatype
* Add strong language support for regular expressions
* Persistent datatypes

## Version 0.4

* Add a unitest construct in Satie ala D
* Basic set of standard libraries
* Basic REPL
* Basic package manager

## Version 0.5

* Interpreter optimizations (not JIT)
* Multi-core support

## Version 0.6

* JIT
