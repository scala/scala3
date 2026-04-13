// Param x in A will be renamed so it doesn't clash. 

inline trait A(x: Int)

class C(x: String) extends A(10):
  val y = x

@main def Test = 
  val v = C("Hello World")
  assert(v.y == "Hello World")
