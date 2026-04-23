inline trait A:
  def x = "Hello world"

class C extends A:
  override val x = "Overridden"

@main def Test = 
  val v = C()
  assert(v.x == "Overridden")
