import scala.quoted.*

object Macros {

  inline def matches[A, B](inline a: A, inline b: B): Unit = ${impl('a, 'b)}

  private def impl[A, B](a: Expr[A], b: Expr[B])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*

    val res = quotes.asInstanceOf[scala.quoted.runtime.QuoteMatching].ExprMatch.unapply[Tuple, Tuple](a)(using b).map { tup =>
      tup.toArray.toList.map {
        case r: Expr[_] =>
          s"Expr(${r.show})"
        case t: Type[_] =>
          s"Type(${Type.show(using t)})"
      }
    }

    '{
      println("Scrutinee: " + ${Expr(a.asTerm.show)})
      println("Pattern: " + ${Expr(b.asTerm.show)})
      println("Result: " + ${Expr(res.toString)})
      println()
    }
  }

}
