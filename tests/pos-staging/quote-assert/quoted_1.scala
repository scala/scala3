import scala.quoted._

object Macros {
  def assertImpl(using s: Scope)(expr: s.Expr[Boolean]) =
    '{ if !($expr) then throw new AssertionError(s"failed assertion: ${$expr}") }
}
