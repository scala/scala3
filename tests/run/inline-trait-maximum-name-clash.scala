inline trait A(x: Int):
  val z = x
inline trait B(x: Int):
  val y = x
inline trait C(val x: Int)

class D(x: Int, z: Int) extends A(314), B(1200), C(12):
  override val y = x
  val w = y

@main def Test = 
  val v = D(100, 3)
  assert(v.y == 100)
  assert(v.w == 100)
  assert(v.z == 314)
  assert(v.x == 12)
