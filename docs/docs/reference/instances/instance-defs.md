---
layout: doc-page
title: "Instance Definitions"
---

Instance definitions provide a concise and uniform syntax for defining implicit values. Example:

```scala
trait Ord[T] {
  def (x: T) compareTo (y: T): Int
  def (x: T) < (y: T) = x.compareTo(y) < 0
  def (x: T) > (y: T) = x.compareTo(y) > 0
}

instance IntOrd of Ord[Int] {
  def (x: Int) compareTo (y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

instance ListOrd[T: Ord] for Ord[List[T]] {
  def (xs: List[T]) compareTo (ys: List[T]): Int = (xs, ys) match {
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => +1
    case (x :: xs1, y :: ys1) =>
      val fst = x.compareTo(y)
      if (fst != 0) fst else xs1.compareTo(ys1)
  }
}
```
Instance definitions can be seen as shorthands for what is currently expressed with implicit object and method definitions.
For instance, the definition of instance `IntOrd` above defines an implicit value of type `Ord[Int]`. It is hence equivalent
to the following implicit object definition:
```scala
implicit object IntOrd extends Ord[Int] {
  def (x: Int) compareTo (y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}
```
The definition of instance `ListOrd` defines an ordering for `List[T]` provided there is an ordering for type `T`. With existing
implicits, this could be expressed as a pair of a class and an implicit method:
```scala
class ListOrd[T: Ord] extends Ord[List[T]] {
  def (xs: List[T]) compareTo (ys: List[T]): Int = (xs, ys) match {
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
## Instances for Extension Methods

Instances can also be defined without an `of` clause. A typical application is to use a instance to package some extension methods. Examples:

```scala
instance StringOps {
  def (xs: Seq[String]) longestStrings: Seq[String] = {
    val maxLength = xs.map(_.length).max
    xs.filter(_.length == maxLength)
  }
}

instance ListOps {
  def (xs: List[T]) second[T] = xs.tail.head
}
```
## Anonymous Instances

The name of an instance definition can be left out. Examples:
```scala
instance of Ord[Int] { ... }
instance [T: Ord] of Ord[List[T]] { ... }

instance {
  def (xs: List[T]) second[T] = xs.tail.head
}
```
If the name of an instance is missing, the compiler will synthesize a name from
the type in the of clause, or, if that is missing, from the first defined
extension method.

## Conditional Instances

An instance definition can depend on another instance being defined. Example:
```scala
trait Conversion[-From, +To] {
  def apply(x: From): To
}

instance [S, T] with (c: Conversion[S, T]) of Conversion[List[S], List[T]] {
  def convert(x: List[From]): List[To] = x.map(c.apply)
}
```
This defines an implicit conversion from `List[S]` to `List[T]` provided there is an implicit conversion from `S` to `T`.
The `with` clause instance defines required instances. The instance of `Conversion[List[From], List[To]]` above is defined only if an instance of `Conversion[From, To]` exists.

Context bounds in instance definitions also translate to implicit parameters, and therefore they can be represented alternatively as with clauses. For instance, here is an equivalent definition of the `ListOrd` instance:
```scala
instance ListOrd[T] with (ord: Ord[T]) of List[Ord[T]] { ... }
```
The name of a parameter in a `with` clause can also be left out, as shown in the following variant of `ListOrd`:
```scala
instance ListOrd[T] with Ord[T] of List[Ord[T]] { ... }
```
As usual one can then infer to implicit parameter only indirectly, by passing it as implicit argument to another function.

## Typeclass Instances

Instance definitions allow a concise and natural expression of typeclasses.
Here are some examples of standard typeclass instances:

Semigroups and monoids:

```scala
trait SemiGroup[T] {
  def (x: T) combine (y: T): T
}
trait Monoid[T] extends SemiGroup[T] {
  def unit: T
}
object Monoid {
  def apply[T] = implicitly[Monoid[T]]
}

instance of Monoid[String] {
  def (x: String) combine (y: String): String = x.concat(y)
  def unit: String = ""
}

def sum[T: Monoid](xs: List[T]): T =
    xs.foldLeft(Monoid[T].unit)(_.combine(_))
```
Functors and monads:
```scala
trait Functor[F[_]] {
  def (x: F[A]) map[A, B] (f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def (x: F[A]) flatMap[A, B] (f: A => F[B]): F[B]
  def (x: F[A]) map[A, B] (f: A => B) = x.flatMap(f `andThen` pure)

  def pure[A](x: A): F[A]
}

instance ListMonad of Monad[List] {
  def (xs: List[A]) flatMap[A, B] (f: A => List[B]): List[B] =
    xs.flatMap(f)
  def pure[A](x: A): List[A] =
    List(x)
}

instance ReaderMonad[Ctx] of Monad[[X] => Ctx => X] {
  def (r: Ctx => A) flatMap[A, B] (f: A => Ctx => B): Ctx => B =
    ctx => f(r(ctx))(ctx)
  def pure[A](x: A): Ctx => A =
    ctx => x
}
```

## Syntax

Here is the new syntax of instance definitions, seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
TmplDef          ::=  ...
                  |  ‘instance’ InstanceDef
InstanceDef      ::=  [id] InstanceParams [‘of’ ConstrApps] [TemplateBody]
InstanceParams   ::=  [DefTypeParamClause] {InstParamClause}
InstParamClause  ::=  ‘with’ (‘(’ [DefParams] ‘)’ | ParamTypes)
ParamTypes       ::=  InfixType {‘,’ InfixType}
```
The identifier `id` can be omitted only if either the `of` part or the template body is present. If the `of` part is missing, the template body must define at least one extension method.