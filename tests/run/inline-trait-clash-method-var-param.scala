inline trait A:
    def x = "Hello world"

class C extends A:
  var x = "Overridden"

@main def Test = 
  val v = C()
  assert(v.x == "Overridden")
