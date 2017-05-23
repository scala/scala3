---
layout: doc-page
title: "Implicit By-Name Parameters"
---

Call-by-name implicit parameters can be used to avoid a divergent implicit expansion.

```scala
trait Codec[T] {
  def write(x: T): Unit
}

implicit def intCodec: Codec[Int] = ???

implicit def optionCodec[T]
    (implicit ev: => Codec[T]): Codec[Option[T]] =
  new {
    def write(xo: Option[T]) = xo match {
      case Some(x) => ev.write(x)
      case None =>
    }
  }

val s = implicitly[Codec[Option[Int]]]

s.write(Some(33))
s.write(None)
```
As is the case for a normal by-name parameter, the argument for the implicit parameter `ev`
is evaluated on demand. In the example above, if the option value `x` is `None`, it is
not evaluated at all.

The synthesized argument for an implicit parameter is backed by a lazy
val if this is necessary to prevent an otherwise diverging expansion.

The precise steps for constructing an implicit argument for a by-name parameter of type `=> T` are as follows.

 1. Create a new implicit value with a fresh name _lv_, which has the signature of the following definition:

    ```scala
    implicit lazy val lv: T
    ```

    The current implementation uses the prefix `$lazy_implicit$` followed by a unique integer for _lv_.

 1. This lazy val is not immediately available as candidate for implicit search (making it immediately available would result in a looping implicit computation). But it becomes available in all nested contexts that look again for an implicit argument to a by-name parameter.

 1. If this implicit search succeeds with expression `E`, and `E` contains references to the lazy implicit value _lv_, replace `E` by


    ```scala
    { implicit lazy val lv: T = E; lv }
    ```

    Otherwise, return `E` unchanged.

In the example above, the definition of `s` would be expanded as follows.

```scala
val s = implicitly[Test.Codec[Option[Int]]](
  optionCodec[Int](intCodec))
```

No lazy val was generated because the synthesized argument is not recursive.
