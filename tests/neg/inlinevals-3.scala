object Test {

  inline def power(x: Double, inline n: Int): Double = // ok
    inline if n == 0 then ??? else ???

  inline val N = 10
  def X = 20

  power(2.0, N) // ok, since it's an inline parameter
  power(2.0, X) // error: cannot reduce inline if

}
