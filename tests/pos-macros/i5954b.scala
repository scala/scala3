abstract class MatcherFactory1 {
  class AndNotWord[A]
}

object MatcherFactory1 {
  import scala.quoted._

  def impl(self: Expr[MatcherFactory1#AndNotWord[Int]])(using QuoteContext) =
    '{ val a: Any = $self }


  def impl[T: Staged](self: Expr[MatcherFactory1#AndNotWord[T]])(using QuoteContext) =
    '{ val a: Any = $self }

}
