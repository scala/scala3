object Test {

  def power0(x: Double, transparent n: Int): Double = ???  // error

  inline def power(x: Double, transparent n: Int): Double = ???  // ok

  transparent val N = 10
  def X = 20

  transparent transparent val twice = 30 // error: repeated modifier

  inline transparent val twice = 30 // error: inline & transparent

  class C(transparent x: Int, private transparent val y: Int) { // error // error
    transparent val foo: Int // error: abstract member may not be inline
    inline def bar: Int // error: abstract member may not be inline
  }

  power(2.0, N) // ok, since it's a by-name parameter
  power(2.0, X) // error: argument to transparent parameter must be a constant expression

  transparent val M = X  // error: rhs must be constant expression

  transparent val xs = List(1, 2, 3) // error: must be a constant expression

  inline def foo(x: Int) = {

    def f(transparent xs: List[Int]) = xs // error

    transparent val y = { println("hi"); 1 }  // ok
    transparent val z = x // ok

  }

  inline def byname(transparent f: => String): Int = ??? // ok

}
