import scala.quoted._

object Macros {

  inline def matches[A, B](inline a: A, inline b: B): Unit = ${impl('a, 'b)}

  private def impl[A, B](a: Expr[A], b: Expr[B])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._

    val res = scala.internal.quoted.Expr.unapply[Tuple, Tuple](a)(using b, qctx).map { tup =>
      tup.toArray.toList.map {
        case r: Expr[_] =>
          s"Expr(${r.show})"
        case t: Type[_] =>
          s"Type(${Type.show(using t)})"
      }
    }

    '{
      println("Scrutinee: " + ${Expr(a.unseal.show)})
      println("Pattern: " + ${Expr(b.unseal.show)})
      println("Result: " + ${Expr(res.toString)})
      println()
    }
  }

}
