import scala.quoted._

object Macros {

  inline def matches[A, B](inline a: A, inline b: B): Unit = ${impl('a, 'b)}

  private def impl[A, B](a: Expr[A], b: Expr[B])(using Quotes) : Expr[Unit] = {
    import quotes.reflect._

    val res = quotes.asInstanceOf[scala.quoted.runtime.QuoteMatching].ExprMatch.unapply[Tuple, Tuple](a)(using b).map { tup =>
      tup.toArray.toList.map {
        case r: Expr[_] =>
          s"Value(${r.show})"
        case t: Type[_] =>
          s"Type(${Type.show(using t)})"
      }
    }

    '{
      println("Scrutinee: " + ${Value(Term.of(a).show)})
      println("Pattern: " + ${Value(Term.of(b).show)})
      println("Result: " + ${Value(res.toString)})
      println()
    }
  }

}
