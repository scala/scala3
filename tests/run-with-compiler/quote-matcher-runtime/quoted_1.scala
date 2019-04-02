import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection

object Macros {

  inline def matches[A, B](a: => A, b: => B): Unit = ${impl('a, 'b)}

  private def impl[A, B](a: Expr[A], b: Expr[B])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._

    val res = scala.runtime.quoted.Matcher.unapply[Tuple](a)(b, reflect).map { tup =>
      tup.toArray.toList.map {
        case r: Expr[_] =>
          s"Expr(${r.unseal.showCode})"
        case r: quoted.Type[_] =>
          s"Type(${r.unseal.showCode})"
      }
    }

    '{
      println("Scrutinee: " + ${a.unseal.showCode.toExpr})
      println("Pattern: " + ${b.unseal.showCode.toExpr})
      println("Result: " + ${res.toString.toExpr})
      println()
    }
  }

}
