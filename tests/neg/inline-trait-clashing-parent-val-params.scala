inline trait A(val x: Int)

inline trait B(val x: Int)

class C extends A(10), B(11) // error: C inherits conflicting members 

@main def Test = 
  val v = C()
  assert(v.x == 11)
