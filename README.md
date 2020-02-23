# aeson-zippy

A small library for zipper-style traversals of Aeson `Value`s.

This library is heavily inspired by the interface [waargonaut](https://github.com/qfpl/waargonaut) provides but functions on top of the existing JSON parsing machinery of [aeson](https://github.com/bos/aeson). While waargonaut offers a very interesting optimization for decoding JSON (succinct data structures) this optimization limits waargonaut to the x86-64 architecture and some of it's dependencies can be difficult to get working out-of-the-box (eg. in nixpkgs); aeson-zippy is a thin layer over aeson and should work everywhere aeson does.

## Objectives

* Small overhead vs. `FromJSON` instances
* Use subset of transitive dependencies of `aeson`

## TODO

* tests
* benchmarks to determine overhead vs. aeson `FromJSON` instances

## References

* Brisbane Functional Programming Group [talk](https://www.youtube.com/watch?v=woK7ntZRwXQ) on zippers
