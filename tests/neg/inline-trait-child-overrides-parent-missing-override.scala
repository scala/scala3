
inline trait A(val x: Int)

class C extends A(10):
  val x = 1000 // error: Needs override modifier
