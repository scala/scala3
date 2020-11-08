import scala.quoted._

object Macros {

  inline def matches[A, B](inline a: A, inline b: B): Unit = ${impl('a, 'b)}

  private def impl[A, B](a: Expr[A], b: Expr[B])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._

    val res = qctx.asInstanceOf[scala.internal.quoted.QuoteContextInternal].ExprMatch.unapply[Tuple, Tuple](a)(using b).map { tup =>
      tup.toArray.toList.map {
        case r: Expr[_] =>
          s"Expr(${r.show})"
        case t: Type[_] =>
          s"Type(${Type.show(using t)})"
      }
    }

    '{
      println("Scrutinee: " + ${Expr(a.asReflectTree.show)})
      println("Pattern: " + ${Expr(b.asReflectTree.show)})
      println("Result: " + ${Expr(res.toString)})
      println()
    }
  }

}
