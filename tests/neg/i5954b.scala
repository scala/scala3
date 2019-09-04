abstract class MatcherFactory1[A] {
  class AndNotWord
}

object MatcherFactory1 {
  import scala.quoted._

  def impl[T](self: Expr[MatcherFactory1[T]#AndNotWord]) given QuoteContext =
    '{ val a: Any = $self } // error: access to type T from wrong staging level

}
