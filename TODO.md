# Milestones and thoughts

## Version 0.1

* [ ] Debug register machine and make Ackermann work (measure performance)
* [ ] Make it possible to specify register content from the `sa` executable
* [ ] Refactor code base
  - Move `vm/src/clib` into `src/clib/`
  - Move `vm/src/*` into `src/sa/`
  - Move `grammar/` into `src/sac/`
  - Add `src/util`
* [ ] Dive into Hindley-Milner type inferens
* [ ] Update `README.md` to a shining condition
* [ ] Design and implement a full set of register instructions
* [ ] Update `sac` compiler to generate register instructions
  - `sa -> ast -> bytecode`
* [ ] Use a dummy GC
* [ ] Use copy-on-write instead of persistent datatypes

## Version 0.1

* Basic set of standard libraries
  - Put them in `lib/`
* Add a unitest construct in Satie ala D
* Basic REPL
* Basic package manager

## Version 0.2

* GChttps://raw.githubusercontent.com/joagre/satie/master/grammar/no_actions_satie.peg

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
