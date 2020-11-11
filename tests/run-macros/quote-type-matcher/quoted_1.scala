import scala.quoted._

object Macros {

  inline def matches[A, B]: Unit = ${ matchesExpr[A, B] }

  private def matchesExpr[A, B](using a: Type[A], b: Type[B])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._

    val res = qctx.asInstanceOf[scala.quoted.internal.QuoteMatching].TypeMatch.unapply[Tuple, Tuple](a)(using b).map { tup =>
      tup.toArray.toList.map {
        case r: Type[_] =>
          s"Type(${TypeTree.of(using r).show})"
      }
    }

    '{
      println("Scrutinee: " + ${Expr(TypeTree.of[A].show)})
      println("Pattern: " + ${Expr(TypeTree.of[B].show)})
      println("Result: " + ${Expr(res.toString)})
      println()
    }
  }

}
