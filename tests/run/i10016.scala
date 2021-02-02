def f(init: Int ?=> Int) : Int = 1
def f(s: String)(init: Int ?=> Int) : Int = 2

@main def Test() =
  assert(f((x: Int) ?=> x) == 1)
