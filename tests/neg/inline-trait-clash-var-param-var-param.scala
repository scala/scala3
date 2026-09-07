//> using options -language:experimental.inlineTraits
inline trait A(val x: Int, var y: Int)
class C(override var y: Int) extends A(10, 54): // error: Cannot override a mutable variable
  override val x = 1000

@main def Test = 
  val v = C(44)
  assert(v.x == 1000)
  assert(v.y == 44)
