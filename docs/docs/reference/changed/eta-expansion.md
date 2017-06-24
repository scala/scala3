---
layout: doc-page
title: "Automatic Eta Expansion"
---

Previously, a method reference `m` was converted to a function value
only if the expected type was a function type. If that was not the
case, one had to write `m _` to force the conversion (which is called
eta-expansion).

For methods with one or more parameters, this restriction has now been
dropped. Example:

    def m(x: Boolean, y: String)(z: Int): List[Int]
    val f1 = m
    val f2 = m(true, "abc")

This creates two function values:

    f1: (Boolean, String) => Int => List[Int]
    f2: Int => List[Int]

The syntax `m _` is no longer needed and will be deprecated in the
future.

Automatic eta expansion does not apply to "nullary" methods that take an empty parameter list. Given

    def next(): T

, a simple reference to `next` does not auto-convert to a
function. One has to write explicitely `() => next()` to achieve that
(it's better to write it this way rather than `next _` because the latter
will be deprecated).

The reason for excluding nullary methods from automatic eta expansion
is that Scala implicitly inserts the `()` argument, which would
conflict with eta expansion. Automatic `()` insertion is
[limited](../dropped/auto-apply.md) in Dotty, but the fundamental ambiguity
remains.

### Reference

For more info, see [PR #2701](https://github.com/lampepfl/dotty/pull/2701).

