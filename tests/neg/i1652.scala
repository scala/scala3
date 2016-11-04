object Test {
  val v: Array[Array[Array]] = Array()    // error // error
  def f[T](w: Array[Array[T]]) = { for (r <- w) () }
  f(v) // error
}
