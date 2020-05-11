import scala.quoted._
object Test
  def run(using s: Scope)(tree: s.tasty.Tree): Unit =
    def nested()(using s1: s.Nested): s1.Expr[Int] =
      '{  ${ makeExpr(tree) } + 1  }
    '{  ${ nested() } + 2 }

  def makeExpr(using s: Scope)(tree: s.tasty.Tree): s.Expr[Int] = ???
