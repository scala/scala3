import scala.quoted._

class Test {
  def fold[W: Type](s: Expr[W]): Quotes ?=> Expr[W] =
    '{ ???; $s }
}
