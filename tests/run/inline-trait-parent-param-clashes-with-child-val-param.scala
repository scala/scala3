// Here we need to rename A.x to avoid a clash 

inline trait A(x: Int)

class C(val x: Int) extends A(10)

@main def Test = 
  val v = C(5)
  assert(v.x == 5)
