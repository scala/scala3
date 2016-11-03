object Test {
  val v: Array[Array[Array]] = Array()    // happens because of the kindedness error here.
  def f[T](w: Array[Array[T]]) = { for (r <- w) () }
  f(v)
}
