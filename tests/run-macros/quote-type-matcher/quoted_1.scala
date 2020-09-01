import scala.quoted._

object Macros {

  inline def matches[A, B]: Unit = ${ matchesExpr(Type[A], Type[B]) }

  private def matchesExpr[A, B](a: Type[A], b: Type[B])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty._

    val res = scala.internal.quoted.Type.unapply[Tuple, Tuple](a)(using b, true, qctx).map { tup =>
      tup.toArray.toList.map {
        case r: quoted.Type[_] =>
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
