import scala.quoted._

class MatchFactory1[T, S[_]] {
  def f: Int = 2
}

object MatcherFactory1 {

  def impl[T, S[_], M >: MatchFactory1[T, S] <: MatchFactory1[T, S]](using s: Scope)(self: s.Expr[M])(using s.Type[T], s.Type[S[T]], s.Type[M]) =
    '{ val a = ${self}; a.f }

}
