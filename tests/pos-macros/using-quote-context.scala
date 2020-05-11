import scala.quoted._

class Test {
  def fold[W](using s: Scope)(x: s.Expr[W])(using s.Type[W]): s.Expr[W] =
    '{ ???; $x }
}
