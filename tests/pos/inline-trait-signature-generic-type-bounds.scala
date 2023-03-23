inline trait A[T <: AnyVal >: Int]:
  def f(x: T): T = x

class B extends A[Int]
