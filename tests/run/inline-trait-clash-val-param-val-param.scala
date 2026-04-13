inline trait A(val x: Int, val y: Int)
class C(override val y: Int) extends A(10, 54):
  override val x = 1000

@main def Test = 
  val v = C(44)
  assert(v.x == 1000)
  assert(v.y == 44)
