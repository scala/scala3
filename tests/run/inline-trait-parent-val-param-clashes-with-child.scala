
inline trait A(val x: Int)

class C extends A(10):
  override val x = 1000

@main def Test = 
  val v = C()
  assert(v.x == 1000)
