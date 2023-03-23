inline trait A:
  class Inner[T <: Int]:
    val x: T = 1

class B extends A:
  def f = Inner[Int]().x