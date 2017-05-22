# Implicit By-Name Parameters

Call-by-name implicit parameters can be used to avoid a divergent implicit expansion.

```scala
implicit def serializeOption[T](implicit ev: => Serializable[T]): Serializable[Option[T]] =
  new Serializable[Option[T]] {
    def write(x: Option[T], out: DataOutputStream) = t match {
      case Some(x) => ev.write(x)
      case None =>
    }
  }

  val s = serializeOption[Option[Int]]
  s.write(Some(33))
  s.write(None)
```
As is the case for a normal by-name parameter, the argument for the implicit parameter `ev`
is evaluated on demand. In the example above, if the option value `x` is `None`, it is
not evaluated at all.

The synthesized argument for an implicit parameter is backed by a lazy
val, which means that the parameter is evaluated at most once. The
lazy val is also available as a value for implicit search, which can
be useful to avoid an otherwise diverging expansion.

The precise steps for constructing an implicit argument for a by-name parameter of type `=> T` are:

 1. Create a new implicit value with a fresh name _lv_, which has the signature of the following definition:

        implicit lazy val lv: T

    The current implementation uses the prefix `$lazy_implicit$` followed by a unique integer for _lv_.

 1. This lazy val is not immediately available as candidate for implicit search (making it immediately available would result in a looping implicit computation). But it becomes available in all nested contexts that look again for an implicit argument to a by-name parameter.

 1. If this implicit search succeeds with expression `E`, and `E` contains references to the lazy implicit value _lv_, replace `E` by

        { implicit lazy val lv: T = E; lv }

    Otherwise, return `E` unchanged.

