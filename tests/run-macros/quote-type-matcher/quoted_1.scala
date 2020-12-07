import scala.quoted._

object Macros {

  inline def matches[A, B]: Unit = ${ matchesExpr[A, B] }

  private def matchesExpr[A, B](using a: Type[A], b: Type[B])(using Quotes) : Expr[Unit] = {
    import quotes.reflect._

    val res = quotes.asInstanceOf[scala.quoted.runtime.QuoteMatching].TypeMatch.unapply[Tuple, Tuple](a)(using b).map { tup =>
      tup.toArray.toList.map {
        case r: Type[_] =>
          s"Type(${TypeTree.of(using r).show})"
      }
    }

    '{
      println("Scrutinee: " + ${Value(TypeTree.of[A].show)})
      println("Pattern: " + ${Value(TypeTree.of[B].show)})
      println("Result: " + ${Value(res.toString)})
      println()
    }
  }

}
