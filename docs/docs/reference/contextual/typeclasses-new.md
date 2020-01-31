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

### Functors:

A `Functor` represents the ability for a type containing zero or more elements, and that can be "mapped over": applying a function to every of its elements.
Let's name our "type containing zero or more elements" `F`. It's a [higher-kinded type](http://guillaume.martres.me/publications/dotty-hk.pdf), since it contains elements of an other type.
Therefore we'll write it `F[_]` since we don't really care about the type of the elements it contains.
The definition of the `Functor` ability would thus be written as:

```scala
trait Functor[F[_]] {
  def map[A, B](original: F[A], mapper: A => B): F[B]
}
```

Which could read as follows: "The `Functor` ability for a wrapper type `F` represents the ability to transform `F[A]` to `F[B]` through the application of the `mapper` function whose type is `A => B`".
This way, we could define an instance of `Functor` for the `List` type: 

```scala
given as Functor[List] {
  def map[A, B](original: List[A], mapper: A => B): List[B] =
    original.map(mapper) // List already has a `map` method
}
```

With this `given` instance in scope, everywhere a `Functor` is expected, the compiler will accept a `List` to be used.

For instance, we may write such a testing method:
```scala
def assertTransformation[F[_]: Functor, A, B](expected: F[B], original: F[A], mapping: A => B): Unit =
  assert(expected == summon[Functor[F]].map(original, mapping))
```

And use it this way, for example:

```scala
assertTransformation(List("a1", "b1"), List("a", "b"), elt => s"${elt}1")
```

That's a first step, but in practice we probably would like the `map` function to be a method directly accessible on the type `F`. So that we can call `map` directly on instances of `F`, and get rid of the `summon[Functor[F]]` part.
As in the previous example of Monoids, [`extension` methods](extension-methods-new.html) help achieving that. Let's re-define the `Functor` _typeclass_ with extension methods.

```scala
trait Functor[F[_]] {
  def [A, B](original: F[A]).map(mapper: A => B): F[B]
}
```

The instance of `Functor` for `List` now becomes:

```scala
given as Functor[List] {
  def [A, B](original: List[A]).map(mapper: A => B): List[B] =
    original.map(mapper) // List already has a `map` method
}
```

It simplifies the `assertTransformation` method:

```scala
def assertTransformation[F[_]: Functor, A, B](expected: F[B], original: F[A], mapping: A => B): Unit =
  assert(expected == original.map(mapping))
```

Since we can now use the `map` method directly accessible on `original` which is of type `F[A]`, where `F` is a `Functor`.


### Monads

Now we have a `Functor` for `List`.

Applying the `List.map` ability with the following mapping function as parameter: `mapping: A => B` would result in a `List[B]`.

Now, applying the `List.map` ability with the following mapping function as parameter: `mapping: A => List[B]` would result in a `List[List[B]]`.  

To avoid avoid managing lists of lists, we may want to "flatten" the values in a single list.

That's where `Monad` enter the party. A `Monad` for type `F[_]` is a `Functor[F]` with 2 more abilities: 
* the flatten ability we just described: turning `F[A]` to `F[B]` when given a `mapping: A => F[B]` function
* the ability to create `F[A]` from a single value `A`

Here is the translation of this definition in Scala 3:

```scala
trait Monad[F[_]] extends Functor[F] { // "A `Monad` for type `F[_]` is a `Functor[F]`" => thus has the `map` ability
  def pure[A](x: A): F[A] // `pure` can construct F[A] from a single value A
  def [A, B](x: F[A]).flatMap(f: A => F[B]): F[B] // the flattening ability is named `flatMap`, using extension methods as previous examples
  def [A, B](x: F[A]).map(f: A => B) = x.flatMap(f `andThen` pure) // the `map(f)` ability is simply a combination of applying `f` then turning the result into an `F[A]` then applying `flatMap` to it
}
```

#### List

Let us declare the `Monad` ability for type `List`  
```scala
given listMonad as Monad[List] {
  def pure[A](x: A): List[A] =
    List(x)
  def [A, B](xs: List[A]).flatMap(f: A => List[B]): List[B] =
    xs.flatMap(f) // let's rely on the existing `flatMap` method of `List`
}
```

`map` implementation is no longer needed.

#### Option

`Option` is an other type having the same kind of behaviour:
* the `map` ability turning `Option[A]` into `Option[B]` if passed a function `f: A => B`
* the `flatMap` ability turning `Option[A]` into `Option[B]` if passed a function `f: A => Option[B]`
* the `pure` ability turning `A` into `Option[A]`

```scala
given optionMonad as Monad[Option] {
  def pure[A](x: A): Option[A] =
    Option(x)
  def [A, B](xs: Option[A]).flatMap(f: A => Option[B]): Option[B] =
    xs.flatMap(f) // let's rely on the existing `flatMap` method of `Option`
}
```

#### The Reader Monad

Another example of a `Monad` is the Reader Monad. It no longer acts on a type like `List` or `Option`, but on a function.
It can be used for example for combining functions that all have need the same type of parameter, for instance, if multiple functions need to access some configuration, context, environment variables, etc.

The Reader monad allows to abstract over such a `Config` dependency (or context, environment, ...), named `Ctx` in the following examples. It is therefore _parameterized_ by `Ctx`:

```scala
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

In addition to these given instances, other constructs like extension methods, context bounds and type lambdas allow a concise and natural expression of _typeclasses_.
