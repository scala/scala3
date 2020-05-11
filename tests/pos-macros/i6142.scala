import scala.quoted._

object O {
  def foo(using s: Scope) = {
    type T
    implicit val _: s.Type[T] = ???
    '[List[T]]
    ()
  }
}
