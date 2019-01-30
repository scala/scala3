---
layout: doc-page
title: "Inferred Instances"
---

Inferred instance definitions provide a concise and uniform syntax for defining values
that can be inferred as implicit arguments. Example:

```scala
trait Ord[T] {
  def (x: T) compareTo (y: T): Int
  def (x: T) < (y: T) = x.compareTo(y) < 0
  def (x: T) > (y: T) = x.compareTo(y) > 0
}

inferred IntOrd for Ord[Int] {
  def (x: Int) compareTo (y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

inferred ListOrd[T: Ord] for Ord[List[T]] {
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
Inferred instance definitions can be seen as shorthands for what is currently expressed with implicit object and method definitions.
For example, the definition of the inferred instance `IntOrd` above defines an implicit value of type `Ord[Int]`. It is hence equivalent
to the following implicit object definition:
```scala
implicit object IntOrd extends Ord[Int] {
  def (x: Int) compareTo (y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}
```
The definition of the inferred instance `ListOrd` defines an ordering for `List[T]` provided there is an ordering for type `T`. With existing
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
## Inferred Instances for Extension Methods

Inferred instances can also be defined without a `for` clause. A typical application is to use an inferred instance to package some extension methods. Examples:

```scala
inferred StringOps {
  def (xs: Seq[String]) longestStrings: Seq[String] = {
    val maxLength = xs.map(_.length).max
    xs.filter(_.length == maxLength)
  }
}

inferred ListOps {
  def (xs: List[T]) second[T] = xs.tail.head
}
```
## Anonymous Inferred Instances

The name of an inferred instance can be left out. Examples:
```scala
inferred for Ord[Int] { ... }
inferred [T: Ord] for Ord[List[T]] { ... }

inferred {
  def (xs: List[T]) second[T] = xs.tail.head
}
```
If the name of an inferred instance is missing, the compiler will synthesize a name from
the type in the of clause, or, if that is missing, from the first defined
extension method.

**Aside: ** Why anonymous inferred instances?

 - It avoids clutter, relieving the programmer from having to invent names that are never referred to.
   Usually the invented names are either meaning less (e.g. `ev1`), or they just rephrase the implemented type.
 - It gives a systematic foundation for synthesized inferred instance definitions, such as those coming from a `derives` clause.
 - It achieves a uniform principle that the name of an implicit is always optional, no matter
   whether the implicit is an inferred instance definition or an implicit parameter.

## Conditional Instances

An inferred instance definition can depend on another inferred instance being defined. Example:
```scala
trait Conversion[-From, +To] {
  def apply(x: From): To
}

inferred [S, T] given (c: Conversion[S, T]) for Conversion[List[S], List[T]] {
  def convert(x: List[From]): List[To] = x.map(c.apply)
}
```
This defines an implicit conversion from `List[S]` to `List[T]` provided there is an implicit conversion from `S` to `T`.
The `given` clause defines required instances. The `Conversion[List[From], List[To]]` instance above
is defined only if a `Conversion[From, To]` instance exists.

Context bounds in inferred instance definitions also translate to implicit parameters,
and therefore they can be represented alternatively as `given` clauses. For example,
here is an equivalent definition of the `ListOrd` instance:
```scala
inferred ListOrd[T] given (ord: Ord[T]) for List[Ord[T]] { ... }
```
The name of a parameter in a `given` clause can also be left out, as shown in the following variant of `ListOrd`:
```scala
inferred ListOrd[T] given Ord[T] for List[Ord[T]] { ... }
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

inferred for Monoid[String] {
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

inferred ListMonad for Monad[List] {
  def (xs: List[A]) flatMap[A, B] (f: A => List[B]): List[B] =
    xs.flatMap(f)
  def pure[A](x: A): List[A] =
    List(x)
}

inferred ReaderMonad[Ctx] for Monad[[X] => Ctx => X] {
  def (r: Ctx => A) flatMap[A, B] (f: A => Ctx => B): Ctx => B =
    ctx => f(r(ctx))(ctx)
  def pure[A](x: A): Ctx => A =
    ctx => x
}
```
## Inferred Alias Instances

An inferred alias instance creates an inferred instance that is equal to
some expression. E.g.,
```
inferred ctx for ExecutionContext = currentThreadPool().context
```
Here, we create an inferred instance `ctx` of type `ExecutionContext` that resolves to the
right hand side `currentThreadPool().context`. Each time an inferred instance of `ExecutionContext`
is demanded, the result of evaluating the right-hand side expression is returned. The instance definition is equivalent to the following implicit definition:
```
final implicit def ctx: ExecutionContext = currentThreadPool().context
```
Alias instances may be anonymous, e.g.
```
inferred for Position = enclosingTree.position
```
An inferred alias instance can have type and context parameters just like any other inferred instance definition, but it can only implement a single type.

## Syntax

Here is the new syntax of inferred instance definitions, seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
TmplDef          ::=  ...
                  |  ‘inferred’ InstanceDef
InstanceDef      ::=  [id] InstanceParams InstanceBody
InstanceParams   ::=  [DefTypeParamClause] {InstParamClause}
InstParamClause  ::=  ‘given’ (‘(’ [DefParams] ‘)’ | ContextTypes)
InstanceBody     ::=  [‘for’ ConstrApp {‘,’ ConstrApp }] [TemplateBody]
                   |  ‘for’ Type ‘=’ Expr
ContextTypes     ::=  RefinedType {‘,’ RefinedType}
```
The identifier `id` can be omitted only if either the `for` part or the template body is present.
If the `for` part is missing, the template body must define at least one extension method.
