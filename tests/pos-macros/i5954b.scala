abstract class MatcherFactory1 {
  class AndNotWord[A]
}

object MatcherFactory1 {
  import scala.quoted._

  def impl(using s: Scope)(self: s.Expr[MatcherFactory1#AndNotWord[Int]]) =
    '{ val a: Any = $self }


  def impl[T](using s: Scope)(self: s.Expr[MatcherFactory1#AndNotWord[T]])(using s.Type[T]) =
    '{ val a: Any = $self }

}
