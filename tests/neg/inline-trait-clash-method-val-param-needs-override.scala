inline trait A:
  def x = "Hello world"

class C extends A:
  val x = "Overridden" // error: Needs override

@main def Test = 
  val v = C()
  assert(v.x == "Overridden")
