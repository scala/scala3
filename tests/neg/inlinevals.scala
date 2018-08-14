object Test {

  transparent def power(x: Double, n: Int & Constant): Double = ???  // ok

  transparent val N = 10
  def X = 20

  transparent transparent val twice = 30 // error: repeated modifier

  class C {
    transparent val foo: Int // error: abstract member may not be inline
    transparent def bar: Int // error: abstract member may not be inline
  }

  power(2.0, N) // ok, since parameter is inlined
  power(2.0, X) // error: argument to transparent parameter must be a constant expression

  transparent val M = X  // error: rhs must be constant expression

  transparent val xs = List(1, 2, 3) // error: must be a constant expression

  transparent def byname(f: => String): Int = ??? // ok

}
