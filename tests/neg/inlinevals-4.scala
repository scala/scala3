object Test {

  inline def power(x: Double, inline n: Int): Double = // ok
    inline if n == 0 then ??? else ???

  inline val N = 10
  def X = 20

  inline val M = X  // error: rhs must be constant expression

  inline val xs = List(1, 2, 3) // error: must be a constant expression

}
