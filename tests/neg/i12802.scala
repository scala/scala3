trait M:
  type X
  object X:
    def foo(): X = ???

transparent inline def m(using m: M): m.type = m

def Test1 =
  given M = new M{}
  import m.*     // error: no implicit argument of type M was found
  val x: X = X.foo()
  println(x)
