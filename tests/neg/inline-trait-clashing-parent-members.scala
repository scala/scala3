//> using options -language:experimental.inlineTraits
inline trait A:
  val x = 10

inline trait B:
  val x = 11

class C extends A, B // error: C inherits conflicting members x from A and B 

@main def Test = 
  val v = C()
  assert(v.x == 11)
