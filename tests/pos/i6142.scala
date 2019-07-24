import scala.quoted._

object O {
  def foo given QuoteContext = {
    type T
    implicit val _: scala.quoted.Type[T] = ???
    '[List[T]]
    ()
  }
}
