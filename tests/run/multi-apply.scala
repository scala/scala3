class MultiApply extends (Int => Int) with ((Int, Int) => Int) {
  def apply(x: Int): Int = x + x
  def apply(x: Int, y: Int): Int = x * y
}

@main
def Test = {
  val fun = new MultiApply
  assert(fun(2) == 4)
  assert(fun(2, 3) == 6)
}