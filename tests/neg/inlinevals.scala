object Test {

  def power0(x: Double, inline n: Int): Double = ???  // error: inline modifier can only be used for parameters of inline methods

  inline val N = 10
  def X = 20

  inline inline val twice = 30 // error: repeated modifier

  class C(inline x: Int, private inline val y: Int) { // error // error inline modifier can only be used for parameters of inline methods (both)
    inline val foo: Int
    inline def bar: Int
  }

  inline def foo(x: Int) = {

    def f(inline xs: List[Int]) = xs // error: inline modifier can only be used for parameters of inline methods

    inline val y = { println("hi"); 1 }  // ok
    inline val z = x // ok

  }

  inline def byname(inline f: => String): Int = ??? // ok

}
