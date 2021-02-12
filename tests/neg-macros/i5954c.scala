abstract class MatcherFactory1 {
  class AndNotWord[A]
}

object MatcherFactory1 {
  import scala.quoted.*

  def impl[T](self: Expr[MatcherFactory1#AndNotWord[T]])(using Quotes) =
    '{ val a: Any = $self } // error: access to type T from wrong staging level

}
