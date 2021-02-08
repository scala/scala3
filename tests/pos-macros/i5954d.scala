abstract class MatcherFactory1 {
  type AndNotWord
}

object MatcherFactory1 {
  import scala.quoted.*

  def impl(self: Expr[MatcherFactory1#AndNotWord])(using Quotes) =
    '{ val a: Any = $self }


  def impl2[T: Type](a: MatcherFactory1)(self: Expr[T])(implicit ev: T =:= a.AndNotWord, qctx: Quotes) =
    '{ val a: Any = $self }

}
