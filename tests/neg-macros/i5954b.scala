abstract class MatcherFactory1[A] {
  class AndNotWord
}

object MatcherFactory1 {
  import scala.quoted._

  def impl[T](using s: Scope)(self: s.Expr[MatcherFactory1[T]#AndNotWord]) =
    '{ val a: Any = $self } // error: access to type T from wrong staging level

}
