class Base
class A[T <: Double](val f: T) extends Base

class Test:
  def test() = m1(new A(m2()))

  def m1(x: Base): Unit = {}
  def m2(p: A[? <: Double] = new A(1.0)): Int = 2
