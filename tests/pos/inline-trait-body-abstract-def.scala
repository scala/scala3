//> using options -language:experimental.inlineTraits
inline trait A[T]:
  def x: T

class B extends A[Int]:
  def x = 1
  def f: Int = x