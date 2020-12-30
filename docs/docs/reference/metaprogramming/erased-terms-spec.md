---
layout: doc-page
title: "Erased Terms Spec"
---

## Rules

1. The `erased`{.scala} modifier can appear:
   * At the start of a parameter block of a method, function or class
   * In a method definition
   * In a `val`{.scala} definition (but not `lazy val`{.scala} or `var`{.scala})

    ```scala
    erased val x = ...
    erased def f = ...

    def g(erased x: Int) = ...

    (erased x: Int) => ...
    def h(x: (erased Int) => Int) = ...

    class K(erased x: Int) { ... }
    ```


2. A reference to an `erased`{.scala} definition can only be used
   * Inside the expression of argument to an `erased`{.scala} parameter
   * Inside the body of an `erased`{.scala} `val`{.scala} or `def`{.scala}


3. Functions
   * `(erased x1: T1, x2: T2, ..., xN: TN) => y : (erased T1, T2, ..., TN) => R`{.scala}
   * `(given erased x1: T1, x2: T2, ..., xN: TN) => y: (given erased T1, T2, ..., TN) => R`{.scala}
   * `(given erased T1) => R  <:<  erased T1 => R`{.scala}
   * `(given erased T1, T2) => R  <:< (erased T1, T2) => R`{.scala}
   *  ...

   Note that there is no subtype relation between `(erased T) => R`{.scala} and `T => R` (or `(given erased T) => R`{.scala} and `(given T) => R`{.scala})


4. Eta expansion

   if `def f(erased x: T): U`{.scala} then `f: (erased T) => U`{.scala}.


5. Erasure Semantics
   * All `erased`{.scala} parameters are removed from the function
   * All argument to `erased`{.scala} parameters are not passed to the function
   * All `erased`{.scala} definitions are removed
   * All `(erased T1, T2, ..., TN) => R`{.scala} and `(given erased T1, T2, ..., TN) => R`{.scala} become `() => R`{.scala}


6. Overloading

   Method with `erased`{.scala} parameters will follow the normal overloading constraints after erasure.


7. Overriding
   * Member definitions overriding each other must both be `erased`{.scala} or not be `erased`{.scala}
   * `def foo(x: T): U`{.scala} cannot be overridden by `def foo(erased x: T): U`{.scala} and vice-versa

