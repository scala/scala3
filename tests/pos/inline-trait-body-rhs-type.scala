import scala.annotation.publicInBinary

inline trait A[T](@publicInBinary private[A] val x: T):
  def f: T = x: T
  def g: T = identity[T](x)
  def h: this.type = this: this.type

class B extends A("Hello")
