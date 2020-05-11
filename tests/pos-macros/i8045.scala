import scala.quoted._
object Test
  def run(using s: Scope)(tree: s.tasty.Tree): Unit =
    '{  ${ makeExpr(tree) } + 1  }
  def makeExpr(using s: Scope)(tree: s.tasty.Tree): s.Expr[Int] = ???
