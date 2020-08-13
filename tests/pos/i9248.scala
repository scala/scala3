class C(var a: Int)

extension (c: C)
  def foo: Int = c.a
  def foo_=(a: Int): Unit = c.a = a

val c = C(10)
val test = c.foo
