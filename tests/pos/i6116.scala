class Test {
  def f(x: Int)(y: String): Int = ???
  def f(x: Int)(y: Int): Int = ???

  f(3)("")       // ok
  f(3)(4)        // ok

  def g(x: Int)(y: Int)(z: Int): Int = ???
  def g(x: Int)(y: Int)(z: String): Int = ???

  g(2)(3)(4)     // ok
  g(2)(3)("")    // ok
}

