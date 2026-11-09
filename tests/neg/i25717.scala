import scala.quoted.*

object Crash {
  def macroImpl(using Quotes)(q"a: String"): Expr[Boolean] = ??? // error

  def f(s"a: String") = ??? // error

  def softie(into q"a: String") = ??? // error
}
