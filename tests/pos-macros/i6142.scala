import scala.quoted._

object O {
  def foo(using QuoteContext) = {
    type T
    implicit val _: scala.quoted.Type[T] = ???
    Type.of[List[T]]
    ()
  }
}
