// We allow multiple inline traits to be mixed in with the same member names; we prefer the latest mixed-in name. 

inline trait A:
  val x = 10

inline trait B:
  val x = 11

class C extends A, B 

@main def Test = 
  val v = C()
  assert(v.x == 11)
