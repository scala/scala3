import scala.quoted._
object Test
  def run(using qctx: QuoteContext)(tree: qctx.reflect.Tree): Unit =
    '{  ${ makeExpr(tree) } + 1  }
  def makeExpr(using qctx: QuoteContext)(tree: qctx.reflect.Tree): Expr[Int] = ???
