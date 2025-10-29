def f(init: Int ?=> Int): Int = 1
def f(s: String)(init: Int ?=> Int): Int = 2

def g(init: String ?=> Int)(s: String): Int = init(using s)
def g(init: Int ?=> Int)(j: Int): Int = init(using j)

@main def Test() =
  assert(f((x: Int) ?=> x) == 1)
  assert(g((i: Int) ?=> i+1)(41) == 42, s"${ g((i: Int) ?=> i+1)(41) }")
  //assert(g(summon[Int]+1)(41) == 42, s"${ g((i: Int) ?=> i+1)(41) }")
