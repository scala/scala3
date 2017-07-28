object Test {
  def p(x: Int) = { println(x); x }
  def foo(x1: Int, x2: Int, x3: Int, x4: Int = p(4), x5: Int = p(5)) = 1
  def traceIndented(x1: Int, x2: Int = p(2), x3: Int = p(3), x4: Int = p(4)) = ()

  def main(args: Array[String]) = {
    foo(p(1), p(2), p(3)) // 1 2 3 4 5
    println()
    foo(p(1), x2 = p(2), x3 = p(3)) // 1 2 3 4 5
    println()
    foo(p(1), x3 = p(3), x2 = p(2)) // 1 3 2 4 5
    println()
    foo(p(1), x3 = p(3), x5 = p(5), x2 = p(2)) // 1 3 5 2 4
    println()
    foo(p(1), x3 = p(3), x4 = p(4), x2 = p(2)) // 1 3 4 2 5
    println()

    foo(p(1), x3 = 3, x4 = p(4), x2 = p(2)) // 1 4 2 5
    println()

    def test = { println(0); Test }
    test.foo(p(1), x3 = p(3), x4 = p(4), x2 = p(2)) // 0 1 3 4 2 5
    println()

    { println(0); Test }.foo(p(1), x3 = p(3), x4 = p(4), x2 = p(2)) // 0 1 3 4 2 5
    println()

    traceIndented(p(1), x3 = p(3)) // 1 3 2 4
    println()

    traceIndented(1, x3 = p(3)) // 3 2 4
    println()

    traceIndented(p(1), x3 = 3) // 1 2 4

  }
}