# Milestones and thoughts

## Version 0.1

* [x] Debug register machine and make Ackermann work (measure performance)
* [x] Make it possible to specify register content from the `sa` executable
* [ ] Ask T about clib
  - `lhash_kv_find` does not work for me
  - `-O3` produces compiler warnings
  - Many calls dynarray_size?
  - static inline in dynarray.h?
* [ ] Refactor code base
  - Move `vm/c/clib` to `src/clib/`
  - Move `vm/c/*` into `src/sa/`
  - Move `grammar/` to `src/sac/`
  - Add `src/base`
  - Add `bin/`
  - Move `vm/exmaples` to `examples/`
  - Let `bin/sac` generate `sab` bytecode from `sa` source files
  - Load bytecode instead of source code
* [ ] Dive into Hindley-Milner type inference
* [ ] Update `README.md` to a shining condition
* [ ] Design and implement a full set of register instructions
* [ ] Update `sac` compiler to generate register instructions
  - `sa -> ast -> sab`
* [ ] Use a dummy GC
* [ ] Use copy-on-write instead of persistent datatypes

## Version 0.2

* Basic set of standard libraries
  - Put them in `lib/`
* Add a unitest construct in Satie ala D
* Basic REPL
* Basic package manager

## Version 0.3

* GC

## Version 0.4

* Persistent datatypes

## Version 0.5

* Interpreter optimizations (not JIT)
* Multi-core support

## Version 0.6

* JIT

## Thoughts

* Objects as a shim layer on top of jobs?
* Keep memory footprint small to make it feasible to run on micro
  controllers?
* Add gradual typing?
