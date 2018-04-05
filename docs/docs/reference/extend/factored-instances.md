---
layout: doc-page
title: "Factored Instance Declarations"
---

Sometimes we want to re-use parts of one instance declaration in another. As an example,
let's flesh out the `Functor` - `Monad` example, adding a typeclass trait `Applicative`
for applicative functors in-between.

```scala
trait Functor[A] extends TypeClass {
  def map[B](f: A => B): This[B]
}

trait Applicative[A] extends Functor[A] {
  def map2[B, C](that: This[B])(f: (A, B) => C): This[C]
  common def pure[B](x: B): This[B]

  def mapWith[B](f: This[A => B]): This[B] = map2(f)((x, f) => f(x))
  def map[B](f: A => B): This[B] = mapWith(pure(f))
}

trait Monad[A] extends Applicative[A] {
  def flatMap[B](f: A => This[B]): This[B]
}
```
`Applicative` can be extended in other ways as well. Here is `Traverse`, another common sub-trait of `Applicative`:

```scala
trait Traverse[A] extends Applicative[A] {
  def traverse[B, C[_]: Applicative](f: A => C[B]): C[This[B]]
}
```

The `List` type can be made an instance of both `Monad` and `Traverse` by writing an instance declaration for `List : Monad` and another one for `List : Traverse`.
However, these two declarations would then need to duplicate the definitions of the `Applicative` methods `map2` and `pure`.

Factored instance declarations provide a way to avoid the duplication. The idea is to write three instance declarations, for `Applicative`, `Monad`, and `Traverse`:

```scala
extension ListApplicative[A] for List : Applicative {

  def map[B](f: A => B): List[B] = this.map(f)

  def map2[B, C](that: List[B])(f: (A, B) => C): List[C] =
    for (x <- this; y <- that) yield f(x, y)

  def pure[B]: List[B] = Nil
}

extension ListMonad[A] for List : Monad {

  def flatMap[B](f: A => List[B]): List[B] = this.flatMap(f)
}

extension ListTraverse[A] for List : Traverse {

  def traverse[B, C[_]: Applicative](f: A => C[B]): C[List[B]] = this match {
    case Nil => Applicative.impl[C].pure[List[B]]
    case x :: xs => f(x).map2(xs.traverse(f))(_ :: _)
  }
}
```
In the definitions above, `ListMonad` and `ListTraverse` lack definitions for `map2` and `pure`. These definitions are provided implicitly by forwarding to corresponding definitions in the `ListApplicative` instance declaration. If we had written the forwarders for `ListMonad` explicitly, this would look like the following, alternative definition:

```scala
extension ListMonad[A] for List : Monad {
  def flatMap[B](f: A => List[B]): List[B] = this.flatMap(f)

  // The following can be implicitly inserted:
  def pure[B] = ListApplicative.pure[B]
  def map2[B, C](that: List[B])(f: (A, B) => C): List[C] =
    ListApplicative.inject(this).map2(that)(f)
}
```
The last `map2` implementation looks like it requires an expensive object creation, but compilers or runtimes can usually optimize that away using inlining and escape analysis.