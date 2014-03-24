object Test {
  trait A[T]
  def f(implicit p: A[_]) = null
  implicit val x: A[_] = null
  println(f)
}
