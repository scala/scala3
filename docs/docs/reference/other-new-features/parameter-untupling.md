---
layout: doc-page
title: "Parameter Untupling"
---

Say you have a list of pairs

     val xs: List[(Int, Int)]

and you want to map `xs` to a list of `Int`s so that each pair of numbers is mapped to
their sum. Previously, the best way to do this was with a pattern-matching decomposition:

    xs map {
      case (x, y) => x + y
    }

While correct, this is also inconvenient. Dotty now also allows:

    xs.map {
      (x, y) => x + y
    }

or, equivalently:

    xs.map(_ + _)

Generally, a function value with `n > 1` parameters is converted to a
pattern-matching closure using `case` if the expected type is a unary
function type of the form `((T_1, ..., T_n)) => U`.

### Reference

For more info see:
* [More details](./parameter-untupling-spec.html)
* [Issue #897](https://github.com/lampepfl/dotty/issues/897).
