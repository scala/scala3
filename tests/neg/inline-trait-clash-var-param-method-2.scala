inline trait A(var x: Int)

class D extends A(10):
  override def x = 1000 // error: cannot override a mutable value
