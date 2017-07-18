object Test {
  val v: Array[Array[Array]] = Array()    // error: Array takes type parameters
  def f[T](w: Array[Array[T]]) = { for (r <- w) () }
  f(v)
}
