sealed trait E[S]
final case class I[T, U <: Iterable[T]]() extends E[U]
class Test {
  def test[X](a: E[X]): Unit = {
    a match {
      case I() => ???
    }
  }
}
