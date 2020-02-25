import scala.quoted._

class Test {
  def fold[W: Type](s: Expr[W]): QuoteContext ?=> Expr[W] =
    '{ ???; $s }
}
