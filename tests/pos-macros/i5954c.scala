abstract class MatcherFactory1[A] {
  class AndNotWord
}

object MatcherFactory1 {
  import scala.quoted.*

  def impl(self: Expr[MatcherFactory1[Int]#AndNotWord])(using Quotes) =
    '{ val a: Any = $self }


  def impl[T: Type](self: Expr[MatcherFactory1[T]#AndNotWord])(using Quotes) =
    '{ val a: Any = $self }

}
