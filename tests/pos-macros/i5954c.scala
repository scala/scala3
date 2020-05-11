abstract class MatcherFactory1[A] {
  class AndNotWord
}

object MatcherFactory1 {
  import scala.quoted._

  def impl(using s: Scope)(self: s.Expr[MatcherFactory1[Int]#AndNotWord]) =
    '{ val a: Any = $self }


  def impl[T](using s: Scope)(self: s.Expr[MatcherFactory1[T]#AndNotWord])(using s.Type[T]) =
    '{ val a: Any = $self }

}
