inline trait A(x: Int)

class C extends A(10):
  val y = x // error: Not Found Error
