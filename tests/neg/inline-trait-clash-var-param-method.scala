inline trait A(var x: Int)

class C extends A(10):
  def x = 1000 // error: Needs override marker

class D extends A(10):
  override def x = 1000 // error: cannot override a mutable value
