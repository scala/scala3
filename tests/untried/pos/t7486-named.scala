
object Test {
  def fold(empty: Any) = ()
  implicit val notAnnotatedImplicit: AnyRef{def empty[A]: Any} = new {
    fold(empty = 0)
    def empty[A]: Any = ???
  }
}
