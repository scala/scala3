inline trait A(x: Int)

class C extends A(10):
  def x = 1000

@main def Test = 
  val v = C()
  assert(v.x == 1000)
