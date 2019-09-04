abstract class MatcherFactory1 {
  class AndNotWord[A]
}

object MatcherFactory1 {
  import scala.quoted._

  def impl(self: Expr[MatcherFactory1#AndNotWord[Int]]) given QuoteContext =
    '{ val a: Any = $self }


  def impl[T: Type](self: Expr[MatcherFactory1#AndNotWord[T]]) given QuoteContext =
    '{ val a: Any = $self }

}
