inline trait A:
  class Inner(val x: Int) // error

class B extends A:
  def f = Inner(17).x