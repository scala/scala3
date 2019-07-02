# Divergences between Scala 2 and Dotty
The following issues encountered when compiling Scala 2 code as-is under Dotty:

## Scalafix candidates
- If a method is defined `toSet()`, it cannot be called `toSet`.
- “result type of implicit definition needs to be given explicitly”
- There are no `'Symbol`s in Scala 3, you must construct symbols via `new Symbol("foo")` instead of old `'foo`

## Trivial
- Scala 2.13 libraries cannot be used from Dotty, because the type signatures have Scala version `5.3` but `5.0` is expected.
- To use Scala 2.12 dependencies from SBT with Dotty, explicitly suffix their names with `_2.12`.
- Feature warnings about implicits `scala.language.implicitConversions` are output by default, unlike in Scala 2. This creates noise. Unclear how to turn off.

Implicit conversions must be applied explicitly:

```scala
  implicit def IterablePath[T](s: Iterable[T])(implicit conv: T => RelPath): RelPath = {
    s.foldLeft(rel){_ / conv(_)}
  }
```

Stronger compile time guarantees on variance.  Scala 2 does not assert variance on default parameters to parameters of the function value type.  E.g. in geny:

```Scala
# Dotty
def count(f: A => Boolean = (a: A) => true): Int =
|                                ^^^^^^^^^^^^^^
|covariant type A occurs in contravariant position in type => A => Boolean of method count$default$1
```

Fix:
```Scala
# Dotty
def count[B >: A](f: B => Boolean = (_: B) => true): Int =
```

## Tricky
- Scala 3 macros are completely different from Scala 2 ones, requires a migration strategy of its own
