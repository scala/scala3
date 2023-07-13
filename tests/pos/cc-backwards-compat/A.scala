package p
class A:
  def map(other: Iter): Iter = other
  def pair[T](x: T): (T, T) = (x, x)
