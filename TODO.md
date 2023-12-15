# Milestones and thoughts

## Version 0.1

* Debug register machine and make Ackermann work (measure performance)
* Add it possible to specify regsiter register using the sa executable
* Move grammar/ into src/sac/ and make a byte code compiler
* Move src/ into src/sa for the vm
* Keep src/clib
* Add src/util
* Dive into Hindley-Milner type inferens
* Design a full set of instructions for the regsiter machine suitable for Satie
* Generate byte code instructions from the AST
* Use a dummy GC
* Use copy-on-write instead of persistent datatypes

## Version 0.2

* GC

## Version 0.3

* Persistent datatypes

## Version 0.4

* Multi-core support

## Thoughts

* Objects as a shim layer on top of jobs?
* Keep memory footprint small to make it feasible to run on micro controllers
