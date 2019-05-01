---
layout: doc-page
title: "Implementing Typeclasses"
---

Delegates, extension methods and context bounds
allow a concise and natural expression of _typeclasses_. Typeclasses are just traits
with canonical implementations defined by delegates. Here are some examples of standard typeclasses:

### Semigroups and monoids:

```scala
trait SemiGroup[T] {
  def (x: T) combine (y: T): T
}
trait Monoid[T] extends SemiGroup[T] {
  def unit: T
}
object Monoid {
  def apply[T] given Monoid[T] = the[Monoid[T]]
}

delegate for Monoid[String] {
  def (x: String) combine (y: String): String = x.concat(y)
  def unit: String = ""
}

delegate for Monoid[Int] {
  def (x: Int) combine (y: Int): Int = x + y
  def unit: Int = 0
}

def sum[T: Monoid](xs: List[T]): T =
    xs.foldLeft(Monoid[T].unit)(_.combine(_))
```

### Functors and monads:

```scala
trait Functor[F[_]] {
  def (x: F[A]) map [A, B] (f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def (x: F[A]) flatMap [A, B] (f: A => F[B]): F[B]
  def (x: F[A]) map [A, B] (f: A => B) = x.flatMap(f `andThen` pure)

  def pure[A](x: A): F[A]
}

delegate ListMonad for Monad[List] {
  def (xs: List[A]) flatMap [A, B] (f: A => List[B]): List[B] =
    xs.flatMap(f)
  def pure[A](x: A): List[A] =
    List(x)
}

delegate ReaderMonad[Ctx] for Monad[[X] => Ctx => X] {
  def (r: Ctx => A) flatMap [A, B] (f: A => Ctx => B): Ctx => B =
    ctx => f(r(ctx))(ctx)
  def pure[A](x: A): Ctx => A =
    ctx => x
}
```
