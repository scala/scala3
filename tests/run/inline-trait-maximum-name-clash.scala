//> using options -language:experimental.inlineTraits
inline trait A(x: Int):
  val z = x
inline trait B(x: Int):
  val y = x
inline trait C(val x: Int)

class D extends A(314), B(1200), C(12):
  override val x = 1
  override val y = x
  val w = y

@main def Test = 
  val v = D()
  assert(v.y == 1)
  assert(v.w == 1)
  assert(v.z == 314)
  assert(v.x == 1)
