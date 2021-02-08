abstract class MatcherFactory1 {
  class AndNotWord[A]
}

object MatcherFactory1 {
  import scala.quoted.*

  def impl(self: Expr[MatcherFactory1#AndNotWord[Int]])(using Quotes) =
    '{ val a: Any = $self }


  def impl[T: Type](self: Expr[MatcherFactory1#AndNotWord[T]])(using Quotes) =
    '{ val a: Any = $self }

}
