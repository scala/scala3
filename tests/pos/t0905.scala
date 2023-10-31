object Test {
  trait A[T]
  def f(implicit p: A[?]) = null
  implicit val x: A[?] = null
  println(f)
}
