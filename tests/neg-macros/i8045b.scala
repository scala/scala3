import scala.quoted.*
object Test
  def run(using q: Quotes)(tree: q.reflect.Tree): Unit =
    def nested()(using q.Nested): Expr[Int] =
      '{  ${ makeExpr(tree) } + 1  } // error
    '{  ${ nested() } + 2 }

  def makeExpr(using q: Quotes)(tree: q.reflect.Tree): Expr[Int] = ???
