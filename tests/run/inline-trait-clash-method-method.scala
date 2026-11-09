//> using options -language:experimental.inlineTraits
inline trait A:
  def x(y: String) = "Hello world"

class C extends A:
  override def x(y: String) = "Hello world2"
  
@main def Test = 
  val v = C()
  assert(v.x("Hello World") == "Hello world2")
