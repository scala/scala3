---
layout: doc-page
title: "Algebraic Data Types"
nightlyOf: https://docs.scala-lang.org/scala3/reference/enums/adts.html
---

The [`enum` concept](./enums.md) is general enough to also support algebraic data types (ADTs) and their generalized version (GADTs).
Here is an example how an `Option` type can be represented as an ADT:

```scala
enum Option[+T]:
  case Some(x: T)
  case None
```

This example introduces an `Option` enum with a covariant type parameter `T` consisting of two cases, `Some` and `None`.
`Some` is parameterized with a value parameter `x`.
It is a shorthand for writing a case class that extends `Option`.
Since `None` is not parameterized, it is treated as a normal enum value.

The `extends` clauses that were omitted in the example above can also be given explicitly:

```scala
enum Option[+T]:
  case Some(x: T) extends Option[T]
  case None       extends Option[Nothing]
```

Note that the parent type of the `None` value is inferred as `Option[Nothing]`.
Generally, all covariant type parameters of the enum class are minimized in a compiler-generated `extends` clause whereas all contravariant type parameters are maximized.
If `Option` was non-variant, you would need to give the extends clause of `None` explicitly.

As for normal enum values, the cases of an `enum` are all defined in the `enum`'s companion object.
So it's `Option.Some` and `Option.None` unless the definitions are "pulled out" with an import.


## Widening of Constructor Application

Observe here the inferred result types of the following expressions:
```scala
scala> Option.Some("hello")
val res1: t2.Option[String] = Some(hello)

scala> Option.None
val res2: t2.Option[Nothing] = None
```

Note that the type of the expressions above is always `Option`.
Generally, the type of a enum case constructor application will be widened to the underlying enum type, unless a more specific type is expected.
This is a subtle difference with respect to normal case classes.
The classes making up the cases do exist, and can be unveiled, either by constructing them directly with a `new`, or by explicitly providing an expected type.

```scala
scala> new Option.Some(2)
val res3: Option.Some[Int] = Some(2)
scala> val x: Option.Some[Int] = Option.Some(3)
val res4: Option.Some[Int] = Some(3)
```

As all other enums, ADTs can define methods.
For instance, here is `Option` again, with an `isDefined` method and an `Option(...)` constructor in its companion object.

```scala
enum Option[+T]:
  case Some(x: T)
  case None

  def isDefined: Boolean = this match
    case None => false
    case _    => true

object Option:

  def apply[T >: Null](x: T): Option[T] =
    if x == null then None else Some(x)

end Option
```

Enumerations and ADTs have been presented as two different concepts.
But since they share the same syntactic construct, they can be seen simply as two ends of a spectrum and it is perfectly possible to construct hybrids.
For instance, the code below gives an implementation of `Color` either with three enum values or with a parameterized case that takes an RGB value.

```scala
enum Color(val rgb: Int):
  case Red   extends Color(0xFF0000)
  case Green extends Color(0x00FF00)
  case Blue  extends Color(0x0000FF)
  case Mix(mix: Int) extends Color(mix)
```
