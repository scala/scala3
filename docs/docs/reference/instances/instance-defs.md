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

instance ListOrd[T: Ord] of Ord[List[T]] {
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

Instance can be seen as shorthands for what is currently expressed as implicit definitions. The instance definitions above could also have been formulated as implicits as follows:
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
In fact, a plausible compilation strategy would map the instance definitions given above to exactly these implicit definitions.

Implicit definitions are kept for the moment but should be be deprecated eventually. As we will see, the only kind of implicit definitions that cannot be directly emulated by instance definitions are implicit conversions.

Why prefer instance over implicit definitions? Their definitions are shorter, more uniform, and they focus on intent rather than mechanism: I.e. we define an _instance of_ a type, instead of an _implicit object_ that happens to _extend_ a type. Likewise, the `ListOrd` instance is shorter and clearer than the class/implicit def combo that emulates it. Arguably, `implicit` was always a misnomer. An `implicit object` is every bit as explicit as a plain object, it's just that the former is eligible as a synthesized (implicit) argument to an _implicit parameter_. So, "implicit" makes sense as an adjective for arguments and at a stretch for parameters, but not so much for the other kinds of definitions.

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
Instances like these translate to `implicit` objects without an extends clause.

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
extension method. Details remain to be specified.

## Conditional Instances

An instance definition can depend on another instance being defined. For instance:
```scala
trait Convertible[From, To] {
  def convert(x: From): To
}

instance [From, To] with (c: Convertible[From, To]) of Convertible[List[From], List[To]] {
  def convert(x: List[From]): List[To] = x.map(c.convert)
}
```

The `with` clause instance defines required instances. The instance of `Convertible[List[From], List[To]]` above is defined only if an instance of `Convertible[From, To]` exists.
`with` clauses translate to implicit parameters of implicit methods. Here is the expansion of the anonymous instance above in terms of a class and an implicit method (the example demonstrates well the reduction in boilerplate afforded by instance syntax):
```scala
class Convertible_List_List_instance[From, To](implicit c: Convertible[From, To])
extends Convertible[List[From], List[To]] {
  def convert (x: List[From]): List[To] = x.map(c.convert)
}
implicit def Convertible_List_List_instance[From, To](implicit c: Convertible[From, To])
  : Convertible[List[From], List[To]] =
  new Convertible_List_List_instance[From, To]
```
Context bounds in instance definitions also translate to implicit parameters, and therefore they can be represented alternatively as with clauses. For instance, here is an equivalent definition of the `ListOrd` instance:
```scala
instance ListOrd[T] with (ord: Ord[T]) of List[Ord[T]] { ... }
```
An underscore ‘_’ can be used as the name of a required instance, if that instance does not
need to be referred to directly. For instance, the last `ListOrd` instance could also have been written like this:
```scala
instance ListOrd[T] with (_: Ord[T]) of List[Ord[T]] { ... }
```

**Design note:** An alternative to the underscore syntax would be to allow the `name:` part to be left out entirely. I.e. it would then be `instance ListOrd[T] with (Ord[T]) of ...`. I am not yet sure which is preferable.


## Typeclass Instances

Here are some examples of standard typeclass instances:

Semigroups and monoids:

```scala
trait SemiGroup[T] {
  def (x: T) combine (y: T): T
}
trait Monoid[T] extends SemiGroup[T] {
  def unit: T
}

instance of Monoid[String] {
  def (x: String) combine (y: String): String = x.concat(y)
  def unit: String = ""
}

def sum[T: Monoid](xs: List[T]): T =
    xs.foldLeft(summon[Monoid[T]].unit)(_.combine(_))
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
InstanceParams   ::=  [DefTypeParamClause] {‘with’ ‘(’ [DefParams] ‘)}
```
The identifier `id` can be omitted only if either the `of` part or the template body is present. If the `of` part is missing, the template body must define at least one extension method.