---
layout: doc-page
title: "Erased Definitions - More Details"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/erased-defs-spec.html
---

## Rules

1. `erased` is a soft modifier. It can appear in a `val` definition or in a parameter.
   * `erased` cannot appear in a `lazy` `val` definition.
   * `erased` _can_ appear for a parameterless given that expands to a value
     definition. In that case the `given` is expanded to a non-lazy `val`.
   * `erased` cannot appear in a call-by-name parameter.
   * `erased` cannot appear in a mutable `var` definition.
   * `erased` cannot appear in an `object` definition.

2. Values or parameters that have a type that extends the `scala.compiletime.Erased` trait are
   implicitly `erased`.

   * The restrictions of point (1) apply.
   * Parameterless givens are treated like values.
   * Mutable variables cannot have a time that extends `scala.compiletime.Erased`.

3. A reference to an `erased` value can only be used in an *erased context*:
   * Inside the expression of an argument to an `erased` parameter
   * Inside the body of an `erased` `val`
   * Inside the path of a dependent type expression

4. `erased` can also be used in a function type, e.g.

   * `(erased T1, T2) => R`
   * `(x: T1, y: erased T2) ?=> T`

   Note that there is no subtype relation between `(erased T) => R` and `T => R` (or `(erased T) ?=> R` and `T ?=> R`). The `erased` parameters must match exactly in their respective positions.

5. Eta expansion

   if `def f(erased x: T): U` then `f: (erased T) => U`.

6. Erasure semantics
   * All `erased` parameters are removed from the function
   * All arguments to `erased` parameters are not passed to the function
   * All `erased` value definitions are removed
   * All `erased` argument types are removed from a function type

7. Overloading

   Method with `erased` parameters will follow the normal overloading constraints after erasure.

8. Overriding
   * Member definitions overriding each other must both be `erased` or not be `erased`.
   * `def foo(x: T): U` cannot be overridden by `def foo(erased x: T): U` and vice-versa.

9. Type Restrictions
   * Polymorphic function literals with erased parameters are currently not supported, and will be rejected by the compiler. This is purely an implementation restriction, and might be lifted in the future.
