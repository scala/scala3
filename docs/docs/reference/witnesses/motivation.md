---
layout: doc-page
title: "Motivation"
---

### Critique

Scala's implicits are its most distinguished feature. They are _the_ fundamental way to abstract over context. They represent a single concept with an extremely varied number of use cases, among them: implementing type classes, establishing context, dependency injection, expressing capabilities, computing new types and proving relationships between them.

At the same time, implicits are also a controversal feature. I believe there are several reasons for this.

First, being very powerful, implicits are easily over-used and mis-used. This observation holds in almomst all cases when we talk about _implicit conversions_, which, even though conceptually different, share the same syntax with other implicit definitions. For instance,
regarding the two definitions

    implicit def i1(implicit x: T): C[T] = ...
    implicit def i2(x: T): C[T] = ...

the first of these is a conditional implicit _value_, the second an implicit _conversion_. Conditional implicit values are a cornerstone for expressing type classes, whereas most applications of implicit conversions have turned out to be of dubious value. The problem is that many newcomers to the language start with defining implicit conversions since they are easy to understand and seem powerful and convenient. Scala 3 will put under a language flag both definitions and applications of "undisciplined" implicit conversions between types defined elsewhere. This is a useful step to push back against overuse of implicit conversions. But the problem remains that syntactically, conversions and values just look too similar for comfort.

Second, implicits pose challenges for tooling. The set of available implicits depends on context, so command completion has to take context into account. This is feasible in an IDE but docs like ScalaDoc that are based static web pages can only provide an approximation. Another problem is that failed implicit searches often give very unspecific error messages, in particular if some deep recursion of a failed implicit search has failed. The dotty compiler implements some improvements in this case, but further progress would be desirable.

Third, the syntax of implicit definitions is maybe a bit too minimal. It consists of a single modifier, `implicit`, that can be attached to a large number of language constructs. A problem with this for newcomers is that it often conveys mechanism better than intent. For instance, a typeclass instance is an implicit object or val if unconditional and an implicit def with implicit parameters if conditional. This describes precisely what the implicit definitions translate to -- just drop the `implicit` modifier, and that's it! But the cues that define intent are rather indirect and can be easily misread, as demonstrated by the definitions of `i1` and `i2` above.

Fourth, the syntax of implicit parameters has also some shortcomings. It starts with the position of `implicit` as a pseudo-modifier that applies to a whole parameter section instead of a single parameter. This represents an irregular case wrt to the rest of Scala's syntax. Furthermore, while implicit _parameters_ are designated specifically, arguments are not. Passing an argument to an implicit parameter looks like a regular application `f(arg)`. This is problematic because it means there can be confusion regarding what parameter gets instantiated in a call. For instance, in
```scala
def currentMap(implicit ctx: Context): Map[String, Int]
```
one cannot write `currentMap("abc")` since the string "abc" is taken as explicit argument to the implicit `ctx` parameter. One has to write `currentMap.apply("abc")` instead, which is awkward and irregular. For the same reason, a method definition can only have one implicit parameter section and it must always come last. This restriction not only reduces orthogonality, but also prevents some useful program constructs, such as a method with a regular parameter type that depends on an implicit value. Finally, it's also a bit annoying that implicit parameters must have a name, even though in most cases that name is never referenced.

None of the shortcomings is fatal, after all implicits are very widely used, and many libraries and applications rely on them. But together, they make code using implicits more cumbersome and less clear than it could be.

Can implicit function types help? Implicit function types allow to abstract over implicit parameterization. They are a key part of the program to make as many aspects of methods as possible first class. Implicit function types can avoid much of the repetition in programs that use implicits widely. But they do not directly address the issues mentioned here.

### Alternative Design

`implicit` is a modifier that gets attached to various constructs. I.e. we talk about implicit vals, defs, objects, parameters, or arguments. This conveys mechanism rather than intent. What _is_ the intent that we want to convey? Ultimately it's "trade types for terms". The programmer specifies a type and the compiler fills in the term matching that type automatically. So the concept we are after would serve to express definitions that provide the canonical instances for certain types.

I believe a good name for is concept is _witness_. A term is a witness for a type by defining an implicit instance of this type. It's secondary whether this
instance takes the form of a `val` or `object` or whether it is a method. It would be better to have a uniform syntax for all of these kinds of instances. The next sections elaborate
such an alternative design.


```scala
trait Ord[T] {
  def compareTo(this x: T)(y: T): Int
  def < (this x: T)(y: T) = x.compareTo(y) < 0
  def > (this x: T)(y: T) = x.compareTo(y) > 0
}

witness IntOrd for Ord[Int] {
  def compareTo(this x: Int)(y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

witness ListOrd[T: Ord] for Ord[List[T]] {
  def compareTo(this xs: List[T])(ys: List[T]): Int = (xs, ys) match {
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => +1
    case (x :: xs1, y :: ys1) =>
      val fst = x.compareTo(y)
      if (fst != 0) fst else xs1.compareTo(ys1)
  }
}
```
Witness are shorthands for implicit definitions. The winesses above could also have been
formulated as implicits as follows:
```scala
implicit object IntOrd extends Ord[Int] {
  def compareTo(this x: Int)(y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

class ListOrd[T: Ord] for Ord[List[T]] {
  def compareTo(this xs: List[T])(ys: List[T]): Int = (xs, ys) match {
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => +1
    case (x :: xs1, y :: ys1) =>
      val fst = x.compareTo(y)
      if (fst != 0) fst else xs1.compareTo(ys1)
  }
}
implicit def ListOrd[T: Ord]: Ord[List[T]] = new ListOrd[T]
```
In fact, a plausible compilation strategy would map the witnesses given above to exactly these implicit definitions.

### Witness Parameters

Witnesses can also have implicit value parameters. For instance, here is an alternative way to write the`ListOrd` witness:
```scala
witness ListOrd[T] with Ord[T] for Ord[List[T]] { ... }

def max[T](xs: List[T]) with (Coercible[T, U]): T = ...

```

### Motivation

Given that witnesses are only a thin veneer on top of implicits, why introduce them?
There are several reasons.

 1. Convey meaning instead of mechanism. Witnesses



trait SemiGroup[T] {
  def combine(this x: T)(y: T): T
}
trait Monoid[T] extends SemiGroup[T] {
  def unit: T
}

witness StringMonoid for Monoid[String] {
  def combine(this x: String)(y: String): String = x.concat(y)
  def unit: String = ""
}
```



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

When is an extension method considered? There are two possibilities. The first (and recommended one) is by defining the extension method as a member of an implicit value. The method can then be used as an extension method wherever the implicit value is applicable. The second possibility is by making the extension method itself visible under a simple name, typically by importing it. As an example, consider an extension method `longestStrings` on `String`. We can either define it like this:


```scala
implicit object StringSeqOps1 {
  def longestStrings(this xs: Seq[String]) = {
    val maxLength = xs.map(_.length).max
    xs.filter(_.length == maxLength)
  }
}
```
Then
```scala
List("here", "is", "a", "list").longestStrings
```
is legal everywhere `StringSeqOps1` is available as an implicit value. Alternatively, we can define `longestStrings`
as a member of a normal object. But then the method has to be brought into scope to be usable as an extension method.

```scala
object StringSeqOps2{
  def longestStrings(this xs: Seq[String]) = {
    val maxLength = xs.map(_.length).max
    xs.filter(_.length == maxLength)
  }
}
import StringSeqOps2.longestStrings
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

**Note**: The translation of extension methods is formulated on method calls. It is thus indepenent from the way infix operations are translated to method calls. For instamce,
if `+:` was formulated as an extension method, it would still have the `this` parameter come first, even though, seen as an operator, `+:` is right-binding:
```scala
def +: [T](this xs: Seq[T))(x: T): Seq[T]
```

### Generic Extensions

The `StringSeqOps` examples extended a specific instance of a generic type. It is also possible
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




