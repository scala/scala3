---
layout: doc-page
title: "Inferable By-Name Parameters"
---

Inferable by-name parameters can be used to avoid a divergent inferred expansion. Example:

```scala
trait Codec[T] {
  def write(x: T): Unit
}

instance intCodec of Codec[Int] = ???

instance optionCodec[T] of Codec[Option[T]] given (ev: => Codec[T]) {
  def write(xo: Option[T]) = xo match {
    case Some(x) => ev.write(x)
    case None =>
  }
}

val s = the[Codec[Option[Int]]]

s.write(Some(33))
s.write(None)
```
As is the case for a normal by-name parameter, the argument for the inferable parameter `ev`
is evaluated on demand. In the example above, if the option value `x` is `None`, it is
not evaluated at all.

The synthesized argument for an inferable parameter is backed by a local val
if this is necessary to prevent an otherwise diverging expansion.

The precise steps for constructing an inferable argument for a by-name parameter of type `=> T` are as follows.

 1. Create a new implicit instance of type `T`:

    ```scala
    instance lv of T = ???
    ```
    where `lv` is an arbitrary fresh name.

 1. This instance is not immediately available as candidate for argument inference (making it immediately available could result in a loop in the synthesized computation). But it becomes available in all nested contexts that look again for an inferred argument to a by-name parameter.

 1. If this search succeeds with expression `E`, and `E` contains references to `lv`, replace `E` by


    ```scala
    { instance lv of T = E; lv }
    ```

    Otherwise, return `E` unchanged.

In the example above, the definition of `s` would be expanded as follows.

```scala
val s = the[Test.Codec[Option[Int]]](
  optionCodec[Int](intCodec))
```

No local instance was generated because the synthesized argument is not recursive.

### Reference

For more info, see [Issue #1998](https://github.com/lampepfl/dotty/issues/1998)
and the associated [Scala SIP](https://docs.scala-lang.org/sips/byname-implicits.html).
