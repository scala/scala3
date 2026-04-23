inline trait A:
  def x(y: String) = "Hello world"

class C extends A:
  def x(y: String) = "Hello world2" // error: Needs override
  
@main def Test = 
  val v = C()
  assert(v.x("Hello World") == "Hello world2")
