cd # Milestones and thoughts

## Version 0.1

* [ ] Make sure that all examples/*.sai works
  - Make sure that all sys calls uses register passing
* [ ] Dive into Hindley-Milner type inference
* [ ] Update `README.md` to a shining condition
* [ ] Update `SAI.md` to a shining condition
* [ ] Implement the instructions described in `SAI.md`
* [ ] Update `sac` compiler to generate register instructions
  - `sa -> ast -> sai -> sab`
* [ ] Use a dummy GC
* [ ] Use copy-on-write instead of persistent datatypes

## Version 0.2

* Add a unitest construct in Satie ala D
* Basic set of standard libraries
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
