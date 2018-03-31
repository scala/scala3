---
layout: doc-page
title: "Parameterized Typeclass Traits"
---

Typeclass traits can be parameterized. Example:

```scala
trait Functor[A] extends TypeClass {
  def map[B](f: A => B): This[B]
}

trait Monad[A] extends Functor[A] {
  def flatMap[B](f: A => This[B]): This[B]
  def map[B](f: A => B) = flatMap(f.andThen(pure))

  common def pure[A](x: A): This[A]
}
```
This defines a parameterized typeclass trait `Functor`, extended by a parameterized typeclass trait `Monad`.

In a parameterized typeclass trait, the kind of `This` is the same as the kind of the
trait. For instance, since `Functor` takes a single invariant type parameter, so does
`This` in `Functor`. Parameterized typeclass traits can only be extended by traits of the same kind. A sub-trait has to pass its type parameters to its super-trait
in the order they were defined: Examples:

```scala
trait S[A, +B] extends TypeClass
trait T[X, +Y] extends S[X, Y]         // OK

/*!*/ trait T1[X, Y] extends S[X, Y]   // error: wrong variance
/*!*/ trait T2[X, Y] extends S[Y, X]   // error: wrong parameter order
/*!*/ trait T3[X, Y] extends S[X, Int] // error: not all parameters are passed
```
These restrictions ensure that all participants in a typeclass trait hierarchy have the same notion of `This`.

Other types are adapted analogously to higher-kinded types. For instance, `Monad`
defines the `Impl` type so that it takes a higher-kinded type parameter
```scala
type Impl[T[_]]
```
and its `inject` has the signature given below.
```scala
def inject[A](x: This[A]): $Instance[A]
```

Here is an example of an instance declaration implementing a parameterized typeclass trait. It turns `List` into `Monad`.

```scala
extension ListMonad[A] for List : Monad {
  def flatMap[B](f: A => List[B]): List[B] = this match {
    case x :: xs => f(x) ++ xs.flatMap(f)
    case Nil => Nil
  }
  common def pure[A] = Nil
}
```

Implementations of higher-kinded typeclass traits have to have the same type parameters as the traits they implement, but can also can introduce new type parameters. New type parameters have to follow the type parameters passed along to the typeclass traits. Examples:

```scala
trait T[X, +Y] {
  def unit[A, B: This[A, B]
}
class C[X, +Y, Z] extends T[X, Y]               // OK

extension E1[X, +Y] for I[X, Y] : T[X, Y]       // OK
extension E2[X, +Y, Z] for I[X, Y, Z] : T[X, Y] // OK
```
Implementations are mapped to companions in the usual way after
subtracting any type parameters that are passed on to a parameterized typeclass traits. So the following three expressions would each select a `unit` value of trait `T`:
```scala
  C[String].unit
  E1.unit
  E2[Int].unit
```

Context bounds also extend to higher-kinds. For instance, here is an extension method `flatten` for all types implementing the `Monad` trait:

```scala
extension MonadFlatten[T[_] : Monad] for T[T[A]] {
  def flatten: T[A] = this.flatMap(identity)
}
```

