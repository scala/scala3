abstract class MatcherFactory1 {
  class AndNotWord
}

object MatcherFactory1 {
  import scala.quoted._

  def impl(using s: Scope)(self: s.Expr[MatcherFactory1#AndNotWord]) =
    '{ val a: Any = $self }


  def impl2[T](using s: Scope)(a: MatcherFactory1)(self: s.Expr[T])(using T =:= a.AndNotWord, s.Type[T]) =
    '{ val a: Any = $self }

}
