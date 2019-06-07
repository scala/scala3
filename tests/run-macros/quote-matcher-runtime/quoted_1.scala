import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection

object Macros {

  inline def matches[A, B](a: => A, b: => B): Unit = ${impl('a, 'b)}

  private def impl[A, B](a: Expr[A], b: Expr[B])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect.{Bind => _, _}

    val res = scala.internal.quoted.Matcher.unapply[Tuple](a)(b, reflect).map { tup =>
      tup.toArray.toList.map {
        case r: Expr[_] =>
          s"Expr(${r.unseal.show})"
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
