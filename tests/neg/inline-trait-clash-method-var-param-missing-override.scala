//> using options -language:experimental.inlineTraits
inline trait A:
    def x = "Hello world"

class C extends A:
  var x = "Overridden" // error: Needs override

@main def Test = 
  val v = C()
  assert(v.x == "Overridden")
