import scala.quoted._
object Test
  def run(using Quotes)(tree: qctx.reflect.Tree): Unit =
    '{  ${ makeExpr(tree) } + 1  }
  def makeExpr(using Quotes)(tree: qctx.reflect.Tree): Expr[Int] = ???
