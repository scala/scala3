---
layout: doc-page
title: "Right-Associative Extension Methods: Details"
nightlyOf: https://docs.scala-lang.org/scala3/reference/contextual/right-associative-extension-methods.html
---

<!-- In case the names of the clauses are modified, also modify them in ClassLikeSupport, Desugar, and RefinedPrinter -->

The most general signature an extension method can have is as follows:
  - An optional type clause `leftTyParams`
  - A possibly empty list of using clauses `leadingUsing`
  - A single parameter `leftParam` (in an explicit term clause)
  - A possibly empty list of using clauses `trailingUsing`
  - A name (preceded by the `def` keyword)
  - An optional type clause `rightTyParams`
  - An optional single parameter `rightParam` (in an explicit term clause)
  - Any number of any clauses `rest`

For example:

```scala
  extension [T]                               // <-- leftTyParams
            (using a: A, b: B)(using c: C)    // <-- leadingUsing
            (x: X)                            // <-- leftParam
            (using d: D)                      // <-- trailingUsing
    def +:: [U]                               // <-- rightTyParams
            (y: Y)                            // <-- rightParam
            (using e: E)(z: Z)                // <-- rest
```


An extension method is treated as a right-associative operator
(as in [SLS §6.12.3](https://www.scala-lang.org/files/archive/spec/2.13/06-expressions.html#infix-operations))
if it has a name ending in `:`, and is immediately followed by a
single explicit term parameter (in other words, `rightParam` is present). In the example above, that parameter is `(y: Y)`.

The Scala compiler pre-processes a right-associative infix operation such as `x +: xs`
to `xs.+:(x)` if `x` is a pure expression or a call-by-name parameter and to `val y = x; xs.+:(y)` otherwise. This is necessary since a regular right-associative infix method
is defined in the class of its right operand. To make up for this swap,
the expansion of right-associative extension methods performs the inverse parameter swap. More precisely, if `rightParam` is present, the total parameter sequence
of the extension method's expansion is:

```
    leftTyParams  leadingUsing  rightTyParams  rightParam  leftParam  trailingUsing  rest
```

In other words, we swap `leftParams  trailingUsing` with `rightTyParam  rightParam`.

For instance, the `+::` method above would become

```scala
  <extension> def +:: [T]
                      (using a: A, b: B)(using c: C)
                      [U]
                      (y: Y)
                      (x: X)
                      (using d: D)
                      (using e: E)(z: Z)
```

This expansion has to be kept in mind when writing right-associative extension
methods with inter-parameter dependencies.

An overall simpler design could be obtained if right-associative operators could _only_ be defined as extension methods, and would be disallowed as normal methods. In that case neither arguments nor parameters would have to be swapped. Future versions of Scala should strive to achieve this simplification.
