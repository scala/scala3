abstract class MatcherFactory1[A] {
  class AndNotWord
}

object MatcherFactory1 {
  import scala.quoted.*

  def impl[T](self: Expr[MatcherFactory1[T]#AndNotWord])(using Quotes) =
    '{ val a: Any = $self } // error: access to type T from wrong staging level

}
