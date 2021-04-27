// Unboxed option type using unions + null + opaque.
// Relies on the fact that Null is not a subtype of AnyRef.
// Test suggested by Sébastien Doeraene.

opaque type Nullable[+A <: AnyRef] = A | Null // disjoint by construction!

object Nullable {
  def apply[A <: AnyRef](x: A | Null): Nullable[A] = x

  def some[A <: AnyRef](x: A): Nullable[A] = x
  def none: Nullable[Nothing] = null

  extension [A <: AnyRef](x: Nullable[A])
    def isEmpty: Boolean = x == null

  extension [A <: AnyRef, B <: AnyRef](x: Nullable[A])
    def flatMap(f: A => Nullable[B]): Nullable[B] =
      if (x == null) null
      else f(x)

  val s1: Nullable[String] = "hello"
  val s2: Nullable[String] = null

  s1.isEmpty
  s1.flatMap((x) => true)
}
