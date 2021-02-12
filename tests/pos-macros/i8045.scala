import scala.quoted.*
object Test
  def run(using Quotes)(tree: quotes.reflect.Tree): Unit =
    def makeExpr(tree: quotes.reflect.Tree): Expr[Int] = ???
    '{  ${ makeExpr(tree) } + 1  }
