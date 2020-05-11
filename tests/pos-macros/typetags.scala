import scala.quoted._

object Test {

  def f[T](using s: Scope)(using s.Type[T]) = {
    implicitly[s.Type[Int]]
    implicitly[s.Type[List[Int]]]
    implicitly[s.Type[T]]
    implicitly[s.Type[List[T]]]
  }
}
