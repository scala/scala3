// Allowed. We rename A.x, A.y to avoid a clash. 

inline trait A(x: Int, y: Int)

class C(val x: Int) extends A(10, 100):
  val y: Int = 10

@main def Test = 
  val v = C(5)
  assert(v.x == 5)
  assert(v.y == 10)
