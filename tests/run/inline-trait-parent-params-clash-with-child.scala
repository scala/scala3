// Param x in A will be renamed so it doesn't clash. 

inline trait A(x: Int):
  val y = x

class C(x: Int) extends A(10)

@main def Test = 
  val v = C(5)
  assert(v.y == 10)
