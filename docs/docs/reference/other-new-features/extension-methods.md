---
layout: doc-page
title: "Extension Methods"
---

Extension methods allow one to add methods to a type after the type is defined. Example:

```scala
case class Circle(x: Double, y: Double, radius: Double)

def (c: Circle) circumference: Double = c.radius * math.Pi * 2
```

Like regular methods, extension methods can be invoked with infix `.`:

```scala
  val circle = Circle(0, 0, 1)
  circle.circumference
```

### Translation of Extension Methods

Extension methods are methods that have a parameter clause in front of the defined
identifier. They translate to methods where the leading parameter section is moved
to after the defined identifier. So, the definition of `circumference` above translates
to the plain method, and can also be invoked as such:
```scala
def circumference(c: Circle): Double = c.radius * math.Pi * 2

assert(circle.circumference == circumference(circle))
```

### Translation of Calls to Extension Methods

When is an extension method applicable? There are two possibilities.

 - An extension method is applicable if it is visible under a simple name, by being defined
   or inherited or imported in a scope enclosing the application.
 - An extension method is applicable if it is a member of an eligible implicit value at the point of the application.

As an example, consider an extension method `longestStrings` on `String` defined in a trait `StringSeqOps`.

```scala
trait StringSeqOps {
  def (xs: Seq[String]) longestStrings = {
    val maxLength = xs.map(_.length).max
    xs.filter(_.length == maxLength)
  }
}
```
We can make the extension method available by defining an implicit instance of `StringSeqOps`, like this:
```scala
implicit object ops1 extends StringSeqOps
```
Then
```scala
List("here", "is", "a", "list").longestStrings
```
is legal everywhere `StringSeqOps1` is available as an implicit value. Alternatively, we can define `longestStrings`
as a member of a normal object. But then the method has to be brought into scope to be usable as an extension method.

```scala
object ops2 extends StringSeqOps
import ops2.longestStrings
List("here", "is", "a", "list").longestStrings
```
The precise rules for resolving a selection to an extension method are as follows.

Assume a selection `e.m[Ts]` where `m` is not a member of `e`, where the type arguments `[Ts]` are optional,
and where `T` is the expected type. The following two rewritings are tried in order:

 1. The selection is rewritten to `m[Ts](e)`.
 2. If the first rewriting does not typecheck with expected type `T`, and there is an implicit value `i`
    in either the current scope or in the implicit scope of `T`, and `i` defines an extension
    method named `m`, then selection is expanded to `i.m[Ts](e)`.
    This second rewriting is attempted at the time where the compiler also tries an implicit conversion
    from `T` to a type containing `m`. If there is more than one way of rewriting, an ambiguity error results.

So `circle.circumference` translates to `CircleOps.circumference(circle)`, provided
`circle` has type `Circle` and `CircleOps` is an eligible implicit (i.e. it is visible at the point of call or it is defined in the companion object of `Circle`).

### Operators

The extension method syntax also applies to the definition of operators.
In each case the definition syntax mirrors the way the operator is applied.
Examples:
```
  def (x: String) < (y: String) = ...
  def (x: Elem) +: (xs: Seq[Elem]) = ...

  "ab" + "c"
  1 +: List(2, 3)
```
The two definitions above translate to
```
  def < (x: String)(y: String) = ...
  def +: (xs: Seq[Elem])(x: Elem) = ...
```
Note that swap of the two parameters `x` and `xs` when translating
the right-binding operator `+:` to an extension method. This is analogous
to the implementation of right binding operators as normal methods.

### Generic Extensions

The `StringSeqOps` examples extended a specific instance of a generic type. It is also possible to extend a generic type by adding type parameters to an extension method:

```scala
def (xs: List[T]) second [T] = xs.tail.head
```

or:


```scala
def (xs: List[List[T]]) flattened [T] = xs.foldLeft[List[T]](Nil)(_ ++ _)
```

or:

```scala
def (x: T) + [T : Numeric](y: T): T = implicitly[Numeric[T]].plus(x, y)
```

As usual, type parameters of the extension method follow the defined method name. Nevertheless, such type parameters can already be used in the preceding parameter clause.

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
    def (x: T) ensuring [T](condition: implicit WrappedResult[T] => Boolean): T = {
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

**A note on formatting** Having a parameter section in front of the defined
method name makes it visually harder to discern what is being defined. To address
that problem, it is recommended that the name of an extension method is
preceded by a space and is also followed by a space if there are more parameters
to come.

### Extension Methods and TypeClasses

The rules for expanding extension methods make sure that they work seamlessly with typeclasses. For instance, consider `SemiGroup` and `Monoid`.
```scala
  // Two typeclasses:
  trait SemiGroup[T] {
    def (x: T) combine(y: T): T
  }
  trait Monoid[T] extends SemiGroup[T] {
    def unit: T
  }

  // An instance declaration:
  implicit object StringMonoid extends Monoid[String] {
    def (x: String) combine (y: String): String = x.concat(y)
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
  trait Ord[T]
    def (x: T) compareTo (y: T): Int
    def (x: T) < (y: T) = x.compareTo(y) < 0
    def (x: T) > (y: T) = x.compareTo(y) > 0
    val minimum: T
  }

  implicit object IntOrd extends Ord[Int] {
    def (x: Int) compareTo (y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0
    val minimum = Int.MinValue
  }

  class ListOrd[T: Ord] extends Ord[List[T]] {
    def (xs: List[T]) compareTo (ys: List[T]): Int = (xs, ys) match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
    }
    val minimum: List[T] = Nil
  }
  implicit def ListOrd[T: Ord]: ListOrd[T] = new ListOrd[T]


  def max[T: Ord](x: T, y: T): T = if (x < y) y else x

  def max[T: Ord](xs: List[T]): T = (implicitly[Ord[T]].minimum /: xs)(max(_, _))
```

### Higher Kinds

Extension methods generalize to higher-kinded types without requiring special provisions. Example:

```scala
  trait Functor[F[_]] {
    def (x: F[A]) map [A, B](f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {
    def (x: F[A]) flatMap [A, B](f: A => F[B]): F[B]
    def (x: F[A]) map [A, B](f: A => B) = x.flatMap(f `andThen` pure)

    def pure[A](x: A): F[A]
  }

  implicit object ListMonad extends Monad[List] {
    def (xs: List[A]) flatMap [A, B](f: A => List[B]): List[B] =
      xs.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)
  }

  implicit class ReaderMonad[Ctx] extends Monad[[X] => Ctx => X] {
    def (r: Ctx => A) flatMap [A, B](f: A => Ctx => B): Ctx => B =
      ctx => f(r(ctx))(ctx)
    def pure[A](x: A): Ctx => A =
      ctx => x
  }
```
### Syntax

The required syntax extension just adds one clause for extension methods relative
to the [current syntax](https://github.com/lampepfl/dotty/blob/master/docs/docs/internals/syntax.md).
```
DefSig            ::=  ...
                    |  ‘(’ DefParam ‘)’ [nl] id [DefTypeParamClause] DefParamClauses
```




