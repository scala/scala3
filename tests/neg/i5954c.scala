abstract class MatcherFactory1 {
  class AndNotWord[A]
}

object MatcherFactory1 {
  import scala.quoted._

  def impl[T](self: Expr[MatcherFactory1#AndNotWord[T]])(given QuoteContext) =
    '{ val a: Any = $self } // error: access to type T from wrong staging level

}
