class C[T]

object X:
  def apply(xs: Int*)(d: Double) = Nil
  def apply[T](xs: Int*)(t: C[T])(d: Double) = Nil
  def apply()(d: Double) = List(List(d))
  def apply[T]()(t: C[T])(d: Double) = List(List(d))

for
  List(x) <- X()(.0)
yield
  x
