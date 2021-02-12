import scala.quoted.*
object Test
  def run(using Quotes)(tree: quotes.reflect.Tree): Unit =
    '{  ${ makeExpr(tree) } + 1  } // error
  def makeExpr(using Quotes)(tree: quotes.reflect.Tree): Expr[Int] = ???
