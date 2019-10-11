import scala.quoted._

object O {
  def foo(given QuoteContext) = {
    type T
    implicit val _: TypeTag[T] = ???
    '[List[T]]
    ()
  }
}
