import scala.quoted._

object O {
  def foo(using Quotes) = {
    type T
    implicit val _: scala.quoted.Type[T] = ???
    Type.of[List[T]]
    ()
  }
}
