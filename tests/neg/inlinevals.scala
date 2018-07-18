object Test {

  def power(x: Double, transparent n: Int): Double = ???

  transparent val N = 10
  def X = 20

  transparent transparent val twice = 30 // error: repeated modifier

  class C(transparent x: Int, private transparent val y: Int) { // error // error
    transparent val foo: Int // error: abstract member may not be inline
    transparent def bar: Int // error: abstract member may not be inline
  }

  power(2.0, N) // ok, since it's a by-name parameter
  power(2.0, X) // error: argument to transparent parameter must be a constant expression

  transparent val M = X  // error: rhs must be constant expression

  transparent val xs = List(1, 2, 3) // error: must be a constant expression

  def f(transparent xs: List[Int]) = xs

  f(List(1, 2, 3)) // error: must be a constant expression

  def byname(transparent f: => String): Int = ??? // ok

  byname("hello" ++ " world")

}
