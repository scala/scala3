---
layout: singlepage-overview
scala3: true
title: "Erased Definitions: More Details"
---

TODO: complete
## Rules

1. `erased` is a soft modifier. It can appear:
   * At the start of a parameter block of a method, function or class
   * In a method definition
   * In a `val` definition (but not `lazy val` or `var`)
   * In a `class` or `trait` definition

    ```scala
    erased val x = ...
    erased def f = ...

    def g(erased x: Int) = ...

    (erased x: Int) => ...
    def h(x: (erased Int) => Int) = ...

    class K(erased x: Int) { ... }
    erased class E {}
    ```


2. A reference to an `erased` val or def can only be used
   * Inside the expression of argument to an `erased` parameter
   * Inside the body of an `erased` `val` or `def`


3. Functions
   * `(erased x1: T1, x2: T2, ..., xN: TN) => y : (erased T1, T2, ..., TN) => R`
   * `(given erased x1: T1, x2: T2, ..., xN: TN) => y: (given erased T1, T2, ..., TN) => R`
   * `(given erased T1) => R  <:<  erased T1 => R`
   * `(given erased T1, T2) => R  <:< (erased T1, T2) => R`
   *  ...

   Note that there is no subtype relation between `(erased T) => R` and `T => R` (or `(given erased T) => R` and `(given T) => R`)


4. Eta expansion

   if `def f(erased x: T): U` then `f: (erased T) => U`.


5. Erasure semantics
   * All `erased` parameters are removed from the function
   * All argument to `erased` parameters are not passed to the function
   * All `erased` definitions are removed
   * All `(erased T1, T2, ..., TN) => R` and `(given erased T1, T2, ..., TN) => R` become `() => R`


6. Overloading

   Method with `erased` parameters will follow the normal overloading constraints after erasure.


7. Overriding
   * Member definitions overriding each other must both be `erased` or not be `erased`
   * `def foo(x: T): U` cannot be overridden by `def foo(erased x: T): U` and vice-versa

