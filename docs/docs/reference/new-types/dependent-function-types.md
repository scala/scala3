---
layout: doc-page
title: "Dependent Function Types"
---

A dependent function type describes functions where the result type may depend
on the function's parameter values. Example:

    trait Entry { type Key; val key: Key }

    def extractKey(e: Entry): e.Key = e.key          // a dependent method
    val extractor: (e: Entry) => e.Key = extractKey  // a dependent function value
    //            ║   ⇓ ⇓ ⇓ ⇓ ⇓ ⇓ ⇓   ║
    //            ║     Dependent     ║
    //            ║   Function Type   ║
    //            ╚═══════════════════╝
Scala already has _dependent methods_, i.e. methods where the result
type refers to some of the parameters of the method. Method
`extractKey` is an example. Its result type, `e.key` refers its
parameter `e` (we also say, `e.Key` _depends_ on `e`). But so far it
was not possible to turn such methods into function values, so that
they can be passed as parameters to other functions, or returned as
results. Dependent methods could not be turned into functions simply
because there was no type that could describe them.

In Dotty this is now possible. The type of the `extractor` value above is

    (e: Entry) => e.Key

This type describes function values that take any argument `x` of type
`Entry` and return a result of type `x.Key`.

Recall that a normal function type `A => B` is represented as an
instance of the `Function1` trait (i.e. `Function1[A, B]`) and
analogously for functions with more parameters. Dependent functions
are also represented as instances of these traits, but they get an additional
refinement. In fact, the dependent function type above is just syntactic sugar for

    Function1[Entry, Entry#Key] {
      def apply(e: Entry): e.Key
    }

[More details](./dependent-function-types-spec.md)
