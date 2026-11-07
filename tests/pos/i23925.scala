object test {
  sealed trait A[T] 
  case class O[T](e: A[T]) extends A[Option[T]]

  def opt[U](v: Option[U], a: A[U]) = ???

  def f[T](a: A[T], t: T) = {
    a match {
      case O(i) => opt(t, i)
    }
  }
}
