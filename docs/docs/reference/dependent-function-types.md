---
layout: doc-page
title: "Dependent Function Types"
---

A dependent function type describes functions where the result type may depend
on the function's parameter values. Example:

    class Keyed { type Key; key: Key }

    def extractKey(k: Keyed): k.Key = k.key          // a dependent method
    val extractor: (k: Keyed) => k.Key = extractKey  // a dependent function value

Scala already has _dependent methods_, i.e. methods where the result
type refers to some of the parameters of the method. Method
`extractKey` is an example. Its result type, `k.key` refers its
parameter `k` (we also say, `k.Key` _depends_ on `k`). But so far it
was not possible to turn such methods into function values, so thay
they can be passed as parameters to other functions, or returned as
results. Dependent methods could not be turned into functions simply
because there was no type that could describe them.

In Dotty this is now made possible. The type of the `extractor` value above is

    (k: Keyed) => k.Key

This type describes function values that take an argument `x` of type
`Keyed` and return a result of type `x.Key`.

Recall that a normal function type `A => B` is represented as an
instance of the `Function1` trait (i.e. `Function1[A, B]`) and
analogously for functions with more parameters. Dependent functions
are also represented as instances of these traits, but they get an additional
refinement. In fact, the dependent function type above is just syntactic sugar for

    Function1[Keyed, Keyed#Key] {
      def apply(k: Keyed): k.Key
    }

In general, a dependent functon type `(x1: K1, ..., xN: KN) => R` of arity `N`
translates to

    FunctionN[K1, ..., Kn, R'] {
      def apply(x1: K1, ..., xN: KN): R
    }

where the result type parameter `R` is an upper approximation of the
true result type `R` that does not mention any of the parameters `k1, ..., kN`.
