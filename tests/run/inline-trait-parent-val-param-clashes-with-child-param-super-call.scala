// Need to rename C.x to avoid a clash

inline trait A(val x: Int):
  val y = x

class C(x: Int) extends A(10)
  val z = x

@main def Test = 
  val v = C(5)
  assert(v.y == 10)
  assert(v.x == 10)
  assert(v.z == 5)
