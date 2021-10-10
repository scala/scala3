---
layout: singlepage-overview
scala3: true
title: "Erased Definitions: More Details"
---

TODO: complete
## Rules

1. `erased` is a soft modifier. It can appear:
   * At the start of a parameter block of a method or class
   * In front of a function type or function literal
   * In a method definition
   * In a `val` definition (but not `lazy val` or `var`)
   * In a `class` or `trait` definition

    ```scala
    erased val x = ...
    erased def f = ...

    def g(erased x: Int) = ...

    erased (x: Int) => ...
    def h(x: erased Int => Int) = ...

    class K(erased x: Int) { ... }
    erased class E {}
    ```


2. A reference to an `erased` val or def can only be used
   * Inside the expression of argument to an `erased` parameter
   * Inside the body of an `erased` `val` or `def`


3. Functions

   * `erased(x1: T1, x2: T2, ..., xN: TN) => y` has type
     `erased(T1, T2, ..., TN) => R`
   * `(erased x1: T1, x2: T2, ..., xN: TN) ?=> y` has type
     `erased (T1, T2, ..., TN) ?=> R`

   Note that there is no subtype relation between `erased T => R` and `T => R` (or `erased T ?=> R` and `T ?=> R`)

4. Eta expansion

Erased methods must be fully applied to arguments, they are not automatically eta-expanded.

5. Erasure semantics
   * All `erased` parameters are removed from a method
   * All arguments to `erased` parameters are not passed to the method
   * All `erased` definitions are removed
   * All `erased (T1, T2, ..., TN) => R` and `erased (T1, T2, ..., TN) ?=> R` become `R`

6. Overloading

   Method with `erased` parameters will follow the normal overloading constraints after erasure.


7. Overriding
   * Member definitions overriding each other must both be `erased` or not be `erased`
   * `def foo(x: T): U` cannot be overridden by `def foo(erased x: T): U` and vice-versa

