cd # Milestones and thoughts

## Version 0.1

* [x] Debug register machine and make Ackermann work (measure performance)
* [x] Make it possible to specify register content from the `sa` executable
* [ ] Make sure that all examples/*.sai works
* Let `bin/sac` generate `sab` bytecode from `sa` source files
  - Load bytecode instead of source code




- rename src/sac to src/lint

- Rename bin/sac to bin/lint (maybe using the %value and more)

- Implement a new lib/sac/sac.c

- Rename lib/sa/main.c to lib/sa/sa.c to

- loader.c (part of) , module to lib/sac

- create src/base
      move util.h, log.h, vm, instructions








* [ ] Dive into Hindley-Milner type inference
* [ ] Update `README.md` to a shining condition
* [ ] Update `SAI.md` to a shining condition
* [ ] Implement the instructions described in `SAI.md`
* [ ] Update `sac` compiler to generate register instructions
  - `sa -> ast -> sab`
* [ ] Use a dummy GC
* [ ] Use copy-on-write instead of persistent datatypes

## Version 0.2

* Add a unitest construct in Satie ala D
* Basic set of standard libraries
  - Put them in `lib/`
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
