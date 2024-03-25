inline trait A[T]:
  def f: T = f
  def f(x: T): T = x
  def f[U <: T](x: U, y: T): T = x
end A

class B extends A[Int]
