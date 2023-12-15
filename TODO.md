# Milestones and thoughts

## Version 0.1

* Debug register machine and make Ackermann work (measure performance)
* Make it possible to specify register content from the sa executable
* Move grammar/ into src/sac/ and make into a bytecode compiler
* Refactopr code base
  _ Move vm/src/clib into src/clib/
  _ Move vm/src/* into src/sa/
  _ Move grammar/ into src/sac/
  _ Add src/util
* Dive into Hindley-Milner type inferens
* Update README.md to a shining condition
* Design a full set of instructions
* Let the sac compiler generate instructions (AST -> bytecode)
* Use a dummy GC
* Use copy-on-write instead of persistent datatypes

## Version 0.1

* Basic set of standard libraries
  _ Put them in lib/
* Add a unitest construct in Satie ala D
* Basic REPL
* Basic package manager

## Version 0.2

* GC

## Version 0.3

* Persistent datatypes

## Version 0.4

* Interpreter optimizations (not JIT)
* Multi-core support

## Version 0.5

* JIT

## Thoughts

* Objects as a shim layer on top of jobs?
* Keep memory footprint small to make it feasible to run on micro
  controllers?
* Add gradual typing?
