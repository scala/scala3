//> using options -language:experimental.inlineTraits
// These params should be renamed (as they are private) so no clash

inline trait A(x: Int):
  val y = x

inline trait B(x: Int):
  val z = x

class C extends A(10), B(11) 

@main def Test = 
  val v = C()
  assert(v.y == 10)
  assert(v.z == 11)
