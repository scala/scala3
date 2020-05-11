import scala.quoted._

object Macros {

  inline def matches[A, B]: Unit = ${ matchesExpr('[A], '[B]) }

  private def matchesExpr[A, B](using s: Scope)(a: s.Type[A], b: s.Type[B]): s.Expr[Unit] = {
    import s.tasty._

    val res = scala.internal.quoted.Type.unapply[Tuple, Tuple, A, B](using s)(a)(using b, true).map { tup =>
      tup.toArray.toList.map {
        case r: String =>
          s"String($r)"
        case r =>
          s"Type(${r.asInstanceOf[s.tasty.Tree].show})"
      }
    }

    '{
      println("Scrutinee: " + ${Expr(a.show)})
      println("Pattern: " + ${Expr(b.show)})
      println("Result: " + ${Expr(res.toString)})
      println()
    }
  }

}
