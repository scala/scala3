object Test {

  def power(x: Double, inline n: Int): Double = ???

  inline val N = 10
  def X = 20

  inline inline val twice = 30 // error: repeated modifier

  class C(inline x: Int, private inline val y: Int) {
    inline val foo: Int // error: abstract member may not be inline
    inline def bar: Int // error: abstract member may not be inline
  }

  power(2.0, N) // ok, since it's a by-name parameter
  power(2.0, X) // error: argument to inline parameter must be a constant expression

  inline val M = X  // error: rhs must be constant expression

  inline val xs = List(1, 2, 3) // error: must be a constant expression

  def f(inline xs: List[Int]) = xs

  f(List(1, 2, 3)) // error: must be a constant expression

  def byname(inline f: => String): Int = ??? // ok

  byname("hello" ++ " world")

}
