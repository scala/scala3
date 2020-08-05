import scala.quoted._

object Macros {

  inline def matches[A, B]: Unit = ${ matchesExpr('[A], '[B]) }

  private def matchesExpr[A, B](a: Type[A], b: Type[B])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty._

    val res = scala.internal.quoted.Type.unapply[Tuple, Tuple](a)(using b, true, qctx).map { tup =>
      tup.toArray.toList.map {
        case r: quoted.Type[_] =>
          s"Type(${r.asTypeTree.show})"
        case r: String =>
          s"String($r)"
      }
    }

    '{
      println("Scrutinee: " + ${Expr(a.asTypeTree.show)})
      println("Pattern: " + ${Expr(b.asTypeTree.show)})
      println("Result: " + ${Expr(res.toString)})
      println()
    }
  }

}
