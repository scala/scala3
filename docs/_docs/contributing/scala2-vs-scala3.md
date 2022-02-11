---
layout: doc-page
title: Divergences between Scala 2 and Dotty
---

# Divergences between Scala 2 and Dotty
The following issues encountered when compiling Scala 2 code as-is under Dotty:

## Scalafix candidates
- If a method is defined `toSet()`, it cannot be called `toSet`.
- “result type of implicit definition needs to be given explicitly”
- There are no `'Symbol`s in Scala 3, you must construct symbols via `new Symbol("foo")` instead of old `'foo`

## Trivial
- Scala 2.13 libraries cannot be used from Dotty because the dotty-library is compiled against the 2.12 standard library which is not binary-compatible with the 2.13 one. We can't be compatible with both at the same time.
- To use Scala 2.12 dependencies from SBT with Dotty, use `withDottyCompat` as documented [here](https://github.com/scala/scala3-example-project#getting-your-project-to-compile-with-dotty).
- Feature warnings about implicits `scala.language.implicitConversions` are output by default, unlike in Scala 2. This creates noise. Unclear how to turn off.

Implicit conversions must be applied explicitly:

```scala
implicit def IterablePath[T](s: Iterable[T])(implicit conv: T => RelPath): RelPath = {
  s.foldLeft(rel){_ / conv(_)}
}
```

Stronger compile time guarantees on variance.  Scala 2 does not assert variance on default parameters to parameters of the function value type.  E.g. in geny:

```scala
# Dotty
def count(f: A => Boolean = (a: A) => true): Int =
|                                ^^^^^^^^^^^^^^
|covariant type A occurs in contravariant position in type => A => Boolean of method count$default$1
```

Fix:
```scala
# Dotty
def count[B >: A](f: B => Boolean = (_: B) => true): Int =
```

## Tricky
- Scala 3 macros are completely different from Scala 2 ones, requires a migration strategy of its own
