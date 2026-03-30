// We allow multiple inline traits to be mixed in with the same member names; we prefer the latest mixed-in name. 

inline trait A(val x: Int)

inline trait B(val x: Int)

class C extends A(10), B(11) 

@main def Test = 
  val v = C()
  assert(v.x == 11)
