---
layout: doc-page
title: "Erased Definitions - More Details"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/erased-defs-spec.html
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

    (erased x: Int, y: Int) => ...
    def h(x: (Int, erased Int) => Int) = ...

    class K(erased x: Int) { ... }
    erased class E {}
    ```


2. A reference to an `erased` val or def can only be used
   * Inside the expression of argument to an `erased` parameter
   * Inside the body of an `erased` `val` or `def`


3. Functions
   * `(erased x1: T1, x2: T2, ..., xN: TN) => y : (erased T1, T2, ..., TN) => R`
   * `(using x1: T1, erased x2: T2, ..., xN: TN) => y: (using T1, erased T2, ..., TN) => R`
   * `(using erased T1) => R  <:<  erased T1 => R`
   * `(using T1, erased T2) => R  <:< (T1, erased T2) => R`
   *  ...

   Note that there is no subtype relation between `(erased T) => R` and `T => R` (or `(given erased T) => R` and `(given T) => R`). The `erased` parameters must match exactly in their respective positions.


4. Eta expansion

   if `def f(erased x: T): U` then `f: (erased T) => U`.


5. Erasure semantics
   * All `erased` parameters are removed from the function
   * All argument to `erased` parameters are not passed to the function
   * All `erased` definitions are removed
   * `(erased ET1, erased ET2, T1, ..., erased ETN, TM) => R` are erased to `(T1, ..., TM) => R`.
   * `(given erased ET1, erased ET2, T1, ..., erased ETN, TM) => R` are erased to `(given T1, ..., TM) => R`.


6. Overloading

   Method with `erased` parameters will follow the normal overloading constraints after erasure.


7. Overriding
   * Member definitions overriding each other must both be `erased` or not be `erased`.
   * `def foo(x: T): U` cannot be overridden by `def foo(erased x: T): U` and vice-versa.

8. Type Restrictions
   * For dependent functions, `erased` parameters are limited to realizable types, that is, types that are inhabited by non-null values.
     This restriction stops us from using a bad bound introduced by an erased value, which leads to unsoundness (see #4060).
   * Polymorphic functions with erased parameters are currently not supported, and will be rejected by the compiler. This is purely an implementation restriction, and might be lifted in the future.
