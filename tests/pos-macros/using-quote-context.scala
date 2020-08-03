import scala.quoted._

class Test {
  def fold[W: Staged](s: Expr[W]): QuoteContext ?=> Expr[W] =
    '{ ???; $s }
}
