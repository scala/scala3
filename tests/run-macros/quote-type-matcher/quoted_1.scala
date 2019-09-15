import scala.quoted._
import scala.quoted.matching._


object Macros {

  inline def matches[A, B]: Unit = ${ matchesExpr('[A], '[B]) }

  private def matchesExpr[A, B](a: Type[A], b: Type[B])(given qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty.{Bind => _, _}

    val res = scala.internal.quoted.Type.unapply[Tuple, Tuple](a)(b, true, qctx).map { tup =>
      tup.toArray.toList.map {
        case r: quoted.Type[_] =>
          s"Type(${r.unseal.show})"
        case r: Bind[_] =>
          s"Bind(${r.name})"
      }
    }

    '{
      println("Scrutinee: " + ${a.unseal.show.toExpr})
      println("Pattern: " + ${b.unseal.show.toExpr})
      println("Result: " + ${res.toString.toExpr})
      println()
    }
  }

}
