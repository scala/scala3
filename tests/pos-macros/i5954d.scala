abstract class MatcherFactory1 {
  type AndNotWord
}

object MatcherFactory1 {
  import scala.quoted._

  def impl(using s: Scope)(self: s.Expr[MatcherFactory1#AndNotWord]) =
    '{ val a: Any = $self }


  def impl2[T](using s: Scope)(a: MatcherFactory1)(self: s.Expr[T])(implicit ev: T =:= a.AndNotWord, t: s.Type[T]) =
    '{ val a: Any = $self }

}
