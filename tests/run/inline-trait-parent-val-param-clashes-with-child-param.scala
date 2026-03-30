// Need to rename C.x to avoid a clash

inline trait A(val x: Int):
  val y = x

class C(x: Int) extends A(10)

@main def Test = 
  val v = C(5)
  assert(v.y == 10)
