object Test {
  trait A[T]
  def f(implicit p: A[_]) = ???
  implicit val x: A[_] = ???
  println(f)
}
