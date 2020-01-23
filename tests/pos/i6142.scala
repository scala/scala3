import scala.quoted._

object O {
  def foo with QuoteContext = {
    type T
    implicit val _: scala.quoted.Type[T] = ???
    '[List[T]]
    ()
  }
}
