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

This expansion also introduces some inconsistencies when calling the extension methods in non infix form. The user needs to invert the order of the arguments at call site manually. For instance:

```scala
  extension [T](x: T)
    def *:(xs: List[T]): List[T] = ...

  y.*:(ys) // error when following the parameter definition order
  ys.*:(y)

  *:(y)(ys) // error when following the parameter definition order
  *:(ys)(y)
```

Another limitation of this representation is that it is impossible to pass the
type parameters of the `def` explicitly, (unless called in prefix form). For instance:

```scala
  extension (x: Int)
    def *:[T](xs: List[T]): List[T] = ...

  xs.*:[Int](1) // error when trying to set T explicitly
```

The expansion of right-associative extension methods also affects the order in which contextual parameters can be passed explicitly.

Group extension can also behave unintuitively, in general all extension in a
group are extension on the receiver. Except if one of these extensions is a
right-associative extension method, in which case that one is an extension on the type of its argument. For instance:
```scala
  extension (a: Int)
    def :+(b: Long): Long = ... // extension on Int
    def +:(b: Long): Long = ... // extension on Long
```
