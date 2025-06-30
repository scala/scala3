---
layout: doc-page
title: "Erased Definitions - More Details"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/erased-defs-spec.html
---

## Rules

1. `erased` is a soft modifier. It can appear in a `val` definition or in a parameter.

2. A reference to an `erased` value can only be used
   * Inside the expression of argument to an `erased` parameter
   * Inside the body of an `erased` `val` or `def`

3. `erased` can also be used in a function type, e.g.

   * `(erased T1, T2) => R`
   * `(x: T1, y: erased T2) ?=> T`

   Note that there is no subtype relation between `(erased T) => R` and `T => R` (or `(erased T) ?=> R` and `T ?=> R`). The `erased` parameters must match exactly in their respective positions.

4. Eta expansion

   if `def f(erased x: T): U` then `f: (erased T) => U`.

5. Erasure semantics
   * All `erased` parameters are removed from the function
   * All argument to `erased` parameters are not passed to the function
   * All `erased` value definitions are removed
   * All `erased` argument types are removed from a function type

6. Overloading

   Method with `erased` parameters will follow the normal overloading constraints after erasure.

7. Overriding
   * Member definitions overriding each other must both be `erased` or not be `erased`.
   * `def foo(x: T): U` cannot be overridden by `def foo(erased x: T): U` and vice-versa.

8. Type Restrictions
   * Polymorphic functions with erased parameters are currently not supported, and will be rejected by the compiler. This is purely an implementation restriction, and might be lifted in the future.
