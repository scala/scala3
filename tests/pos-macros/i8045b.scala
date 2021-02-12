import scala.quoted.*
object Test
  def run(using q: Quotes)(tree: q.reflect.Tree): Unit =
    def makeExpr(tree: q.reflect.Tree): Expr[Int] = ???
    def nested()(using q.Nested): Expr[Int] =
      '{  ${ makeExpr(tree) } + 1  }
    '{  ${ nested() } + 2 }

