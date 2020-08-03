import scala.quoted._

object Macros {

  inline def matches[A, B](inline a: A, inline b: B): Unit = ${impl('a, 'b)}

  private def impl[A, B](a: Expr[A], b: Expr[B])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty._

    val res = scala.internal.quoted.Expr.unapply[Tuple, Tuple](a)(using b, true, qctx).map { tup =>
      tup.toArray.toList.map {
        case r: Expr[_] =>
          s"Expr(${r.unseal.show})"
        case r: quoted.Staged[_] =>
          s"Type(${r.unseal.show})"
        case r: String =>
          s"String($r)"
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
