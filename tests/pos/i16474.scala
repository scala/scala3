object o:

  private[o] trait A
  trait B
  class C extends A, B
  class D extends A, B

def test =
  def f[T](x: T): T => T = identity
  val g = f(if ??? then o.C() else o.D())
  g(new o.B{})


