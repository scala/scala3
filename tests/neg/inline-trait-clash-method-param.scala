inline trait A:
    def x(b: Int) = "Hello world"
    def z = "Hello world"

class C(x: String, z: String) extends A: // error: Inlining of inline trait created name conflict on z.
  val y = x
  val w = z

@main def Test = 
  val v = C("Overridden", "Overridden2")
  assert(v.y == "Overridden")
