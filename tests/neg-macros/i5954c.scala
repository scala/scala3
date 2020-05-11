abstract class MatcherFactory1 {
  class AndNotWord[A]
}

object MatcherFactory1 {
  import scala.quoted._

  def impl[T](using s: Scope)(self: s.Expr[MatcherFactory1#AndNotWord[T]]) =
    '{ val a: Any = $self } // error: access to type T from wrong staging level

}
