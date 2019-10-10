import scala.quoted._

class MatchFactory1[T, S[_]] {
  def f: Int = 2
}

object MatcherFactory1 {

  def impl[T: TypeTag, S[_], M >: MatchFactory1[T, S] <: MatchFactory1[T, S] : TypeTag](self: Expr[M])(implicit qctx: QuoteContext, tpS: TypeTag[S[T]]) =
    '{ val a = ${self}; a.f }

}
