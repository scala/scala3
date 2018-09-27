---
layout: doc-page
title: "Extension Methods"
---

Extension methods allow one to add methods to a type after the type is defined. Example:

```scala
case class Circle(x: Double, y: Double, radius: Double)

implicit object CircleOps {
  def circumference(this c: Circle): Double = c.radius * math.Pi * 2
}
```

`CircleOps` adds an extension method `circumference` to values of class `Circle`. Like regular methods, extension methods can be invoked with infix `.`:

```scala
  val circle = Circle(0, 0, 1)
  circle.circumference
```

Extension methods are methods that have a `this` modifier for the first parameter.
They can also be invoked as plain methods. So the following holds:
```scala
assert(circle.circumference == CircleOps.circumference(circle))
```

### Translation of Calls to Extension Methods

The rules for resolving a selection `e.m` are augmented as follows: If `m` is not a
member of the type `T` of `e`, and there is an implicit value `i`
in either the current scope or in the implicit scope of `T`, and `i` defines an extension
method named `m`, then `e.m` is expanded to `i.m(e)`. This expansion is attempted at the time where the compiler also tries an implicit conversion from `T` to a type containing `m`. If there is more than one way
of expanding, an ambiguity error results.

So `circle.circumference` translates to `CircleOps.circumference(circle)`, provided
`circle` has type `Circle` and `CircleOps` is an eligible implicit (i.e. it is visible at the point of call or it is defined in the companion object of `Circle`).

### Extended Types

Extension methods can be added to arbitrary types. For instance, the following
object adds a `longestStrings` extension method to a `Seq[String]`:

```scala
implicit object StringOps {
  def longestStrings(this xs: Seq[String]) = {
    val maxLength = xs.map(_.length).max
    xs.filter(_.length == maxLength)
  }
}
```

### Generic Extensions

The previous example extended a specific instance of a generic type. It is also possible
to extend a generic type by adding type parameters to an extension method:

```scala
implicit object ListOps {
  def second[T](this xs: List[T]) = xs.tail.head
}
```

or:


```scala
implicit object ListListOps {
  def flattened[T](this xs: List[List[T]]) = xs.foldLeft[List[T]](Nil)(_ ++ _)
}
```

As usual, type parameters of the extension method follow the defined method name. Nevertheless, such type parameters can already be used in the parameter clause that precedes the defined method name.

### A Larger Example

As a larger example, here is a way to define constructs for checking arbitrary postconditions using `ensuring` so that the checked result can be referred to simply by `result`. The example combines opaque aliases, implicit function types, and extensions to provide a zero-overhead abstraction.

```scala
object PostConditions {
  opaque type WrappedResult[T] = T

  private object WrappedResult {
    def wrap[T](x: T): WrappedResult[T] = x
    def unwrap[T](x: WrappedResult[T]): T = x
  }

  def result[T](implicit er: WrappedResult[T]): T = WrappedResult.unwrap(er)

  implicit object Ensuring {
    def ensuring[T](this x: T)(condition: implicit WrappedResult[T] => Boolean): T = {
      implicit val wrapped = WrappedResult.wrap(x)
      assert(condition)
      x
    }
  }
}

object Test {
  import PostConditions._
  val s = List(1, 2, 3).sum.ensuring(result == 6)
}
```
**Explanations**: We use an implicit function type `implicit WrappedResult[T] => Boolean`
as the type of the condition of `ensuring`. An argument condition to `ensuring` such as
`(result == 6)` will therefore have an implicit value of type `WrappedResult[T]` in scope
to pass along to the `result` method. `WrappedResult` is a fresh type, to make sure that we do not get unwanted implicits in scope (this is good practice in all cases where implicit parameters are involved). Since `WrappedResult` is an opaque type alias, its values need not be boxed, and since `ensuring` is added as an extension method, its argument does not need boxing either. Hence, the implementation of `ensuring` is as about as efficient as the best possible code one could write by hand:

    { val result = List(1, 2, 3).sum
      assert(result == 6)
      result
    }

### Rules for Overriding Extension Methods

Extension methods may override only extension methods and can be overridden only by extension methods.

### Extension Methods and TypeClasses

The rules for expanding extension methods make sure that they work seamlessly with typeclasses. For instance, consider `SemiGroup` and `Monoid`.
```scala
  // Two typeclasses:
  trait SemiGroup[T] {
    def combine(this x: T)(y: T): T
  }
  trait Monoid[T] extends SemiGroup[T] {
    def unit: T
  }

  // An instance declaration:
  implicit object StringMonoid extends Monoid[String] {
    def combine(this x: String)(y: String): String = x.concat(y)
    def unit: String = ""
  }

  // Abstracting over a typeclass with a context bound:
  def sum[T: Monoid](xs: List[T]): T =
    xs.foldLeft(implicitly[Monoid[T]].unit)(_.combine(_))
```
In the last line, the call to `_.combine(_)` expands to `(x1, x2) => x1.combine(x2)`,
which expands in turn to `(x1, x2) => ev.combine(x1, x2)` where `ev` is the implicit
evidence parameter summoned by the context bound `[T: Monoid]`. This works since
extension methods apply everywhere their enclosing object is available as an implicit.

### Generic Extension Classes

As another example, consider implementations of an `Ord` type class with a `minimum` value:
```scala
  trait Ord[T] {
    def compareTo(this x: T)(y: T): Int
    def < (this x: T)(y: T) = x.compareTo(y) < 0
    def > (this x: T)(y: T) = x.compareTo(y) > 0
    val minimum: T
  }

  implicit object IntOrd extends Ord[Int] {
    def compareTo(this x: Int)(y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0
    val minimum = Int.MinValue
  }

  implicit class ListOrd[T: Ord] extends Ord[List[T]] {
    def compareTo(this xs: List[T])(ys: List[T]): Int = (xs, ys) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
    }
    val minimum: List[T] = Nil
  }

  def max[T: Ord](x: T, y: T): T = if (x < y) y else x

  def max[T: Ord](xs: List[T]): T = (implicitly[Ord[T]].minimum /: xs)(max(_, _))
```
The `ListOrd` class is generic - it works for any type argument `T` that is itself an instance of `Ord`. In current Scala, we could not define `ListOrd` as an implicit class since implicit classes can only define implicit converions that take exactly one non-implicit value parameter. We propose to drop this requirement and to also allow implicit classes without any value parameters, or with only implicit value parameters. The generated implicit method would in each case follow the signature of the class. That is, for `ListOrd` we'd generate the method:
```scala
  implicit def ListOrd[T: Ord]: ListOrd[T] = new ListOrd[T]
```

### Higher Kinds

Extension methods generalize to higher-kinded types without requiring special provisions. Example:

```scala
  trait Functor[F[_]] {
    def map[A, B](this x: F[A])(f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](this x: F[A])(f: A => F[B]): F[B]
    def map[A, B](this x: F[A])(f: A => B) = x.flatMap(f `andThen` pure)

    def pure[A](x: A): F[A]
  }

  implicit object ListMonad extends Monad[List] {
    def flatMap[A, B](this xs: List[A])(f: A => List[B]): List[B] =
      xs.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)
  }

  implicit class ReaderMonad[Ctx] extends Monad[[X] => Ctx => X] {
    def flatMap[A, B](this r: Ctx => A)(f: A => Ctx => B): Ctx => B =
      ctx => f(r(ctx))(ctx)
    def pure[A](x: A): Ctx => A =
      ctx => x
  }
```
### Syntax

The required syntax extension just adds one clause for extension methods relative
to the [current syntax](https://github.com/lampepfl/dotty/blob/master/docs/docs/internals/syntax.md).
```
DefSig          ::=  id [DefTypeParamClause] [ExtParamClause] DefParamClauses
ExtParamClause  ::=  [nl] ‘(’ ‘this’ DefParam ‘)’
```




