---
layout: doc-page
title: "Multiversal Equality"
---

Previously, Scala had universal equality: Two values of any types
could be compared with each other with `==` and `!=`. This came from
the fact that `==` and `!=` are implemented in terms of Java's
`equals` method, which can also compare values of any two reference
types.

Universal equality is convenient but also dangerous since it
undermines type safety. Say you have an erroneous program where
a value `y` has type `S` instead of the expected type `T`.

    val x = ... // of type T
    val y = ... // of type S, but should be T
    x == y      // typechecks, will always yield false

If all you do with `y` is compare it to other values of type `T`, the program will
typecheck but probably give unexpected results.

Multiversal equality is an opt-in way to make universal equality
safer. The idea is that by declaring an `implicit` value one can
restrict the types that are legal in comparisons. The example above
would not typecheck if an implicit was declared like this for type `T`
(or an analogous one for type `S`):

    implicit def eqT: Eq[T, T] = Eq

This definition effectively says that value of type `T` can (only) be
compared with `==` or `!=` to other values of type `T`. The definition
is used only for type checking; it has no significance for runtime
behavior, since `==` always maps to `equals` and `!=` alwatys maps to
the negation of `equals`. The right hand side of the definition is a value
that has any `Eq` instance as its type. Here is the definition of class
`Eq` and its companion object:

    package scala
    import annotation.implicitNotFound

    @implicitNotFound("Values of types ${L} and ${R} cannot be compared with == or !=")
    sealed trait Eq[-L, -R]

    object Eq extends Eq[Any, Any]

One can have several `Eq` instances for a type. For example, the four
definitions below make values of type `A` and type `B` comparable with
each other, but not comparable to anything else:

    implicit def eqA : Eq[A, A] = Eq
    implicit def eqB : Eq[B, B] = Eq
    implicit def eqAB: Eq[A, B] = Eq
    implicit def eqBA: Eq[B, A] = Eq

(As usual, the names of the implicit definitions don't matter, we have
chosen `eqA`, ..., `eqBA` only for illustration).

The `dotty.DottyPredef` object defines a number of `Eq`
implicits. `dotty.DottyPredef` is a temporary `Predef`-like object.
The contents of this object are by default imported into every
program. Once dotty becomes standard Scala, `DottyPredef` will go away
and its contents will be merged with `scala.Predef`.

The `Eq` instances defined by `DottyPredef` make values of types
`String`, `Boolean` and `Unit` only comparable to values of the same
type. They also make numbers only comparable to other numbers, and
sequences only comparable to other sequences. There's also a
"fallback" instance `eqAny` that allows comparisons over types that do
not themeselves have an `Eq` instance.  `eqAny` is defined as follows:

    implicit def eqAny[L, R]: Eq[L, R] = Eq

The primary motivation for having `eqAny` is backwards compatibility,
if this is of no concern one can disable `eqAny` by unimporting it
from `DottyPredef` like this

    import dotty.DottyPredef.{eqAny => _, _}

All `enum` types also come with `Eq` instances that make values of the
`enum` type comparable only to other values of that `enum` type.

The precise rules for equality checking are as follows.

 1. A comparison using `x == y` or `x != y` between values `x: T` and `y: U`
    is legal if either `T` and `U` are the same, or one of the types is a subtype
    of the other, or an implicit value of type `scala.Eq[T, U]` is found.

 2. The usual rules for implicit search apply also to `Eq` instances,
    with one modification: The value `eqAny` in `dotty.DottyPredef` is
    eligible only if neither `T` nor `U` have a reflexive `Eq`
    instance themselves. Here, a type `T` has a reflexive `Eq`
    instance if the implicit search for `Eq[T, T]` where `eqAny` is
    not eligible is successful.

More on multiversal equality is found in a [blog post]
and a [Github issue].

### Reference

For more info, see [Issue #1247](https://github.com/lampepfl/dotty/issues/1247).