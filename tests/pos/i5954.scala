abstract class MatcherFactory1 {
  class AndNotWord
}

object MatcherFactory1 {
  import scala.quoted._

  def impl(self: Expr[MatcherFactory1#AndNotWord]) =
    '{ val a: Any = $self }


  def impl2[T: Type](a: MatcherFactory1)(self: Expr[T])(implicit ev: T =:= a.AndNotWord) =
    '{ val a: Any = $self }

}
