// Unboxed option type using unions + null + opaque.
// Relies on the fact that Null is not a subtype of AnyRef.
// Test suggested by Sébastien Doeraene.

object Nullables {
  opaque type Nullable[+A <: AnyRef] = A | Null // disjoint by construction!

  object Nullable:
    def apply[A <: AnyRef](x: A | Null): Nullable[A] = x

    def some[A <: AnyRef](x: A): Nullable[A] = x
    def none: Nullable[Nothing] = null

  extension [A <: AnyRef](x: Nullable[A])
    def isEmpty: Boolean = x == null
    def get: A | Null = x

  extension [A <: AnyRef, B <: AnyRef](x: Nullable[A])
    def flatMap(f: A => Nullable[B]): Nullable[B] =
      if (x == null) null
      else f(x)

    def map(f: A => B): Nullable[B] = x.flatMap(f)

  def test1 =
    val s1: Nullable[String] = Nullable("hello")
    val s2: Nullable[String] = "world"
    val s3: Nullable[String] = Nullable.none
    val s4: Nullable[String] = null

    s1.isEmpty
    s1.flatMap((x) => true)

    assert(s2 != null)
}

def test2 =
  import Nullables._

  val s1: Nullable[String] = Nullable("hello")
  val s2: Nullable[String] = Nullable.none
  val s3: Nullable[String] = null // error: don't leak nullable union

  s1.isEmpty
  s1.flatMap((x) => Nullable(true))

  assert(s2 == null) // error
