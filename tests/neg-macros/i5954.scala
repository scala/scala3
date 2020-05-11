abstract class MatcherFactory1 {
  class AndNotWord
}

object MatcherFactory1 {
  import scala.quoted._

  def impl2(using s: Scope)(a: MatcherFactory1)(self: s.Expr[a.AndNotWord]) =
    '{ val a: Any = $self } // error: access to value a from wrong staging level
}
