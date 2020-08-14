class C(var a: Int)

extension (c: C)
  def foo: Int = c.a
  def foo_=(a: Int): Unit = c.a = a

val c = C(10)
val c1 = c.foo = 11

given C = C(0)

// Harder case: extensions defined in local scope, with type parameters and implicits
def test =
  class D[T](var a: T)

  extension [T](d: D[T])(using C)
    def foo: T = d.a
    def foo_=(a: T): Unit =
      val c = summon[C]
      d.a = a

  val d = D(10)
  d.foo
  d.foo = 11
