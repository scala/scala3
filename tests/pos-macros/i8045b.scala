import scala.quoted._
object Test
  def run(using qctx: QuoteContext)(tree: qctx.reflect.Tree): Unit =
    def nested()(using qctx.Nested): Expr[Int] =
      '{  ${ makeExpr(tree) } + 1  }
    '{  ${ nested() } + 2 }

  def makeExpr(using qctx: QuoteContext)(tree: qctx.reflect.Tree): Expr[Int] = ???
