// We should rename A.x here to avoid a clash. 

inline trait A(x: Int)

class C extends A(10):
  val x = 5

@main def Test = 
  val v = C()
  assert(v.x == 5)
