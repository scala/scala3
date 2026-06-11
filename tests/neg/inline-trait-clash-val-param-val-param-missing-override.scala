
inline trait A(val x: Int, val y: Int)
class C(val y: Int) extends A(10, 54): // error: Needs override
  val x = 1000 // error: Needs override
