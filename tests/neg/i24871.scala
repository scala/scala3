class C[T]

object X:
  def apply(xs: Int*)(d: Double) = Nil
  def apply[T](xs: Int*)(t: C[T])(d: Double) = Nil
  def apply()(d: Double) = List(List(d))
  def apply[T]()(t: C[T])(d: Double) = List(List(d))

@main def Test = println:
  X()(3.14) // error
