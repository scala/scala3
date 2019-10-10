import scala.quoted._
import scala.quoted.matching._


object Macros {

  inline def matches[A, B](a: => A, b: => B): Unit = ${impl('a, 'b)}

  private def impl[A, B](a: Expr[A], b: Expr[B])(given qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty.{Bind => _, _}

    val res = scala.internal.quoted.Expr.unapply[Tuple, Tuple](a)(b, true, qctx).map { tup =>
      tup.toArray.toList.map {
        case r: Expr[_] =>
          s"Expr(${r.unseal.show})"
        case r: TypeTag[_] =>
          s"TypeTag(${r.unseal.show})"
        case r: Sym[_] =>
          s"Sym(${r.name})"
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
