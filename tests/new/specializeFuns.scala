object Test {

  def foo(f: Int => Int, x: Int) = f(x)

  def main(args: Array[String]) = {
    assert(foo(x => x * x, 4) == 16)
    val xs = List(1, 2, 3)
    assert(xs.map(x => x) == List(1, 2, 3))

    def reduce(f: (Int, Int) => Int) = xs.reduce(f)
    assert(xs.reduce(_ + _) == 6)
    assert(reduce(_ + _) == 6)
  }
}
