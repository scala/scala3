---
layout: singlepage-overview
scala3: true
title: "Implicit Conversions - More Details"
---

## Implementation

An implicit conversion, or _view_, from type `S` to type `T` is
defined by either:

- An `implicit def` which has type `S => T` or `(=> S) => T`
- An implicit value which has type `Conversion[S, T]`

The standard library defines an abstract class `Conversion`:

```scala
package scala
@java.lang.FunctionalInterface
abstract class Conversion[-T, +U] extends Function1[T, U]:
  def apply(x: T): U
```

Function literals are automatically converted to `Conversion` values.

Views are applied in three situations:

1. If an expression `e` is of type `T`, and `T` does not conform to
   the expression's expected type `pt`. In this case, an implicit `v`
   which is applicable to `e` and whose result type conforms to `pt`
   is searched. The search proceeds as in the case of implicit
   parameters, where the implicit scope is the one of `T => pt`. If
   such a view is found, the expression `e` is converted to `v(e)`.
1. In a selection `e.m` with `e` of type `T`, if the selector `m` does
   not denote an accessible member of `T`. In this case, a view `v`
   which is applicable to `e` and whose result contains an accessible
   member named `m` is searched. The search proceeds as in the case of
   implicit parameters, where the implicit scope is the one of `T`. If
   such a view is found, the selection `e.m` is converted to `v(e).m`.
1. In an application `e.m(args)` with `e` of type `T`, if the selector
   `m` denotes some accessible member(s) of `T`, but none of these
   members is applicable to the arguments `args`. In this case, a view
   `v` which is applicable to `e` and whose result contains a method
   `m` which is applicable to `args` is searched. The search proceeds
   as in the case of implicit parameters, where the implicit scope is
   the one of `T`. If such a view is found, the application
   `e.m(args)` is converted to `v(e).m(args)`.

# Differences with Scala 2 implicit conversions

In Scala 2, views whose parameters are passed by-value take precedence
over views whose parameters are passed by-name. This is no longer the
case in Scala 3. A type error reporting the ambiguous conversions will
be emitted in cases where this rule would be applied in Scala 2:

```scala
implicit def conv1(x: Int): String = x.toString
implicit def conv2(x: => Int): String = x.toString

val x: String = 0 // Compiles in Scala2 (uses `conv1`),
                  // type error in Scala 3 because of ambiguity.
```

In Scala 2, implicit values of a function type would be considered as
potential views. In Scala 3, these implicit value need to have type
`Conversion`:

```scala
// Scala 2:
def foo(x: Int)(implicit conv: Int => String): String = x

// Becomes with Scala 3:
def foo(x: Int)(implicit conv: Conversion[Int, String]): String = x

// Call site is unchanged:
foo(4)(_.toString)

// Scala 2:
implicit val myConverter: Int => String = _.toString

// Becomes with Scala 3:
implicit val myConverter: Conversion[Int, String] = _.toString
```

Note that implicit conversions are also affected by the [changes to implicit resolution](implicit-resolution.html) between Scala 2 and Scala 3.

## Motivation for the changes

The introduction of [`scala.Conversion`](https://github.com/lampepfl/dotty/blob/master/library/src/scala/Conversion.scala)
in Scala 3 and the decision to restrict implicit values of this type to be
considered as potential views comes from the desire to remove surprising
behavior from the language:

```scala
implicit val m: Map[Int, String] = Map(1 -> "abc")

val x: String = 1  // Scala 2: assigns "abc" to x
                   // Scala 3: type error
```

This snippet contains a type error. The right-hand side of `val x`
does not conform to type `String`. In Scala 2, the compiler will use
`m` as an implicit conversion from `Int` to `String`, whereas Scala 3
will report a type error, because `Map` isn't an instance of
`Conversion`.

## Migration path

Implicit values that are used as views should see their type changed to `Conversion`.

For the migration of implicit conversions that are affected by the
changes to implicit resolution, refer to the [Changes in Implicit Resolution](implicit-resolution.html) for more information.

## Reference

For more information about implicit resolution, see [Changes in Implicit Resolution](implicit-resolution.html).
Other details are available in [PR #2065](https://github.com/lampepfl/dotty/pull/2065).
