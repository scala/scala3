//> using options -language:experimental.inlineTraits
inline trait A:
  def x = "Hello world"

class C extends A:
  override var x = "Overridden" // error: Setter x_= overrides nothing

@main def Test = 
  val v = C()
  assert(v.x == "Overridden")
