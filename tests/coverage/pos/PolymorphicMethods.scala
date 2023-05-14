package covtest

object PolyMeth:
  def f[A](x: A): A = x
  this.f(0) // (this.f[type])(0) i.e. Apply(TypeApply(Select(this,f), type), List(0))

  C[String]().f("str", 0)

class C[T1]:
  def f[T2](p1: T1, p2: T2): Unit = ()
