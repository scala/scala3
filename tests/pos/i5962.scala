import scala.quoted._
import scala.tasty._

class MatchFactory1[T, S[_]] {
  def f: Int = 2
}

object MatcherFactory1 {

  def impl[T: Type, S[_], M >: MatchFactory1[T, S] <: MatchFactory1[T, S] : Type](self: Expr[M])(implicit refl: Reflection, tpS: Type[S[T]]) =
    '{ val a = ${self}; a.f }

}
