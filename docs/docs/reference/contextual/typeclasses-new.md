---
layout: doc-page
title: "Implementing Typeclasses"
---

In Scala 3, _typeclasses_ are just traits whose implementation are defined by given instances. 
Here are some examples of standard typeclasses:

### Semigroups and monoids:

Here's the `Monoid` typeclass definition:

```scala
trait SemiGroup[T] {
  @infix def (x: T) combine (y: T): T
}

trait Monoid[T] extends SemiGroup[T] {
  def unit: T
}
```

An implementation of this `Monoid` typeclass for the type `String` can be the following: 

```scala
given as Monoid[String] {
  def (x: String) combine (y: String): String = x.concat(y)
  def unit: String = ""
}
```

Whereas for the type `Int` one could write the following:
```scala
given as Monoid[Int] {
  def (x: Int) combine (y: Int): Int = x + y
  def unit: Int = 0
}
```

This monoid can now be used as _context bound_ in the following `combineAll` method:

```scala
def combineAll[T: Monoid](xs: List[T]): T =
    xs.foldLeft(summon[Monoid[T]].unit)(_ combine _)
```

To get rid of the `summon[...]` we can define a `Monoid` object as follows:

```scala
object Monoid {
  def apply[T] with (m: Monoid[T]) = m
}
```

Which would allow to re-write the `combineAll` method this way: 

```scala
def combineAll[T: Monoid](xs: List[T]): T =
    xs.foldLeft(Monoid[T].unit)(_ combine _)
```

We can also benefit from [extension methods](extension-methods-new.html) to make this `combineAll` function accessible as a method on the `List` type:


```scala
def [T: Monoid](xs: List[T]).combineAll: T =
  xs.foldLeft(Monoid[T].unit)(_ combine _)  
```

Which allows one to write: 

```scala
assert("ab" == List("a", "b").combineAll)
```
or:
```scala
assert(3 == List(1, 2).combineAll)
```

### Functors and monads:

```scala
trait Functor[F[_]] {
  def [A, B](x: F[A]).map(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def [A, B](x: F[A]).flatMap(f: A => F[B]): F[B]
  def [A, B](x: F[A]).map(f: A => B) = x.flatMap(f `andThen` pure)

  def pure[A](x: A): F[A]
}

given listMonad as Monad[List] {
  def [A, B](xs: List[A]).flatMap(f: A => List[B]): List[B] =
    xs.flatMap(f)
  def pure[A](x: A): List[A] =
    List(x)
}

given readerMonad[Ctx] as Monad[[X] =>> Ctx => X] {
  def [A, B](r: Ctx => A).flatMap(f: A => Ctx => B): Ctx => B =
    ctx => f(r(ctx))(ctx)
  def pure[A](x: A): Ctx => A =
    ctx => x
}
```

### Summary

The definition of a _typeclass_ is expressed in Scala 3 via a `trait`.
The main difference with other traits resides in how these traits are implemented. 
In the case of a _typeclass_ the trait's implementations are expressed through `given ... as` type definitions, and not through classes that `extends` the trait linearly.

In addition to these given instances, extension methods and context bounds allow a concise and natural expression of _typeclasses_.
