import scala.quoted._

object Macros {

  inline def matches[A, B](inline a: A, inline b: B): Unit = ${impl('a, 'b)}

  private def impl[A, B](using s: Scope)(a: s.Expr[A], b: s.Expr[B]): s.Expr[Unit] = {
    import s.tasty._

    val res = scala.internal.quoted.Expr.unapply[Tuple, Tuple](using s)(a)(using b, true).map { tup =>
      tup.toArray.toList.map {
        case r: String =>
          s"String($r)"
        case r =>
          r.asInstanceOf[s.tasty.Tree] match
            case t: s.tasty.Term => s"Expr(${t.show})"
            case t: s.tasty.TypeTree => s"Type(${t.show})"
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
