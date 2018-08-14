object Test {

  def power0(x: Double, n: Int & Constant): Double = ???  // error

  transparent def power(x: Double, n: Int & Constant): Double = ???  // ok

  transparent val N = 10
  def X = 20

  class C(x: Int & Constant, private val y: Int & Constant) // error // error

  transparent def foo(x: Int) = {

    def f(xs: List[Int] & Constant) = xs // error

    transparent val y = { println("hi"); 1 }  // ok
    transparent val z = x // ok

  }
}
