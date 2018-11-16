---
layout: doc-page
title: "Witness Definitions"
---

Witnesses provide a concise and uniform syntax for defining implicit values. Example:

```scala
trait Ord[T] {
  def (x: T) compareTo (y: T): Int
  def (x: T) < (y: T) = x.compareTo(y) < 0
  def (x: T) > (y: T) = x.compareTo(y) > 0
}

witness IntOrd for Ord[Int] {
  def (x: Int) compareTo (y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

witness ListOrd[T: Ord] for Ord[List[T]] {
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

Witness can be seen as shorthands for what is currently expressed as implicit definitions. The witnesses above could also have been formulated as implicits as follows:
```scala
implicit object IntOrd extends Ord[Int] {
  def (x: Int) compareTo (y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

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
In fact, a plausible compilation strategy would map the witnesses given above to exactly these implicit definitions.

Implicit definitions are kept for the moment but should be be deprecated eventually. As we will see, the only kind of implicit definitions that cannot be directly emulated by witnesses are implicit conversions.

Why prefer witnesses over implicit definitions? Their definitions are shorter, more uniform, and they focus on intent rather than mechanism: I.e. we define a _witness for_ a type, instead of an _implicit object_ that happens to _extend_ a type. Likewise, the `ListOrd` witness is shorter and clearer than the class/implicit def combo that emulates it. Arguably, `implicit` was always a misnomer. An `implicit object` is every bit as explicit as a plain object, it's just that the former is eligible as a synthesized argument to an implicit _parameter_. So, "implicit" makes sense as an adjective for parameters, but not so much for the other kinds of definitions.

## Witnesses for Extension Methods

Witnesses can also be defined without a `for` clause. A typical application is to use a witness to define extension methods. Examples:

```scala
witness StringOps {
  def (xs: Seq[String]) longestStrings: Seq[String] = {
    val maxLength = xs.map(_.length).max
    xs.filter(_.length == maxLength)
  }
}

witness ListOps {
  def (xs: List[T]) second[T] = xs.tail.head
}
```
Witnesses like these translate to `implicit` objects without an extends clause.

## Anonymous Witnesses

The name of a witness definition can be left out. Examples:
```scala
witness for Ord[Int] { ... }
witness [T: Ord] for Ord[List[T]] { ... }

witness {
  def (xs: List[T]) second[T] = xs.tail.head
}
```
If the name of a witness is missing, the compiler will synthesize a name from
the type in the for clause, or, if that is missing, from the first defined
extension method. Details remain to be specified.

## Conditional Witnesses

A witness can depend on another witness being defined. For instance:
```scala
trait Convertible[From, To] {
  def (x: From) convert: To
}

witness [From, To] with (c: Convertible[From, To]) for Convertible[List[From], List[To]] {
  def (x: List[From]) convert: List[To] = x.map(c.convert)
}
```

The `with` clause in a witness defines required witnesses. The witness for `Convertible[List[From], List[To]]` above is defined only if a witness for `Convertible[From, To]` exists.
`with` clauses translate to implicit parameters of implicit methods. Here is the expansion of the anonymous witness above in terms of a class and an implicit method (the example demonstrates well the reduction in boilerplate that witness syntax can achieve):
```scala
class Convertible_List_List_witness[From, To](implicit c: Convertible[From, To])
extends Convertible[List[From], List[To]] {
  def (x: List[From]) convert: List[To] = x.map(c.convert)
}
implicit def Convertible_List_List_witness[From, To](implicit c: Convertible[From, To])
  : Convertible[List[From], List[To]] =
  new Convertible_List_List_witness[From, To]
```
Context bounds in witness definitions also translate to implicit parameters, and therefore they can be represented alternatively as with clauses. For instance, here is an equivalent definition of the `ListOrd` witness:
```scala
witness ListOrd[T] with (ord: Ord[T]) for List[Ord[T]] { ... }
```
An underscore ‘_’ can be used as the name of a required witness, if that witness does not
need to be referred to directly. For instance, the last `ListOrd` witness could also have been written like this:
```scala
witness ListOrd[T] with (_: Ord[T]) for List[Ord[T]] { ... }
```

**Design note:** An alternative to the underscore syntax would be to allow the `name:` part to be left out entirely. I.e. it would then be `witness ListOrd[T] with (Ord[T]) for ...`. I am not yet sure which is preferable.


## Witnesses as Typeclass Instances

Here are some examples of witnesses for standard typeclasses:

Semigroups and monoids:

```scala
trait SemiGroup[T] {
  def (x: T) combine (y: T): T
}
trait Monoid[T] extends SemiGroup[T] {
  def unit: T
}
object Monoid {
  def apply[T: Monoid] = summon[Monoid[T]]
}

witness for Monoid[String] {
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

witness ListMonad for Monad[List] {
  def (xs: List[A]) flatMap[A, B] (f: A => List[B]): List[B] =
    xs.flatMap(f)
  def pure[A](x: A): List[A] =
    List(x)
}

witness ReaderMonad[Ctx] for Monad[[X] => Ctx => X] {
  def (r: Ctx => A) flatMap[A, B] (f: A => Ctx => B): Ctx => B =
    ctx => f(r(ctx))(ctx)
  def pure[A](x: A): Ctx => A =
    ctx => x
}
```

## Syntax

Here is the new syntax for witness definitions, seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
TmplDef         ::=  ...
                  |  ‘witness’ WitnessDef
WitnessDef      ::=  [id] WitnessParams [‘for’ ConstrApps] [TemplateBody]
WitnessParams   ::=  [DefTypeParamClause] {‘with’ ‘(’ [DefParams] ‘)}
```
The identifier `id` can be omitted only if either the `for` part or the template body is present. If the `for` part is missing, the template body must define at least one extension method.