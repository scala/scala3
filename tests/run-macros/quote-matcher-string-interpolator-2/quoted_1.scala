import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection

object Macros {

  inline def (self: => StringContext) xyz(args: => String*): String = ${impl('self, 'args)}

  private def impl(self: Expr[StringContext], args: Expr[Seq[String]])(implicit reflect: Reflection): Expr[String] = {
    (self, args) match {
      case ('{ StringContext(${Repeated(parts)}: _*) }, Repeated(args1)) =>
        val strParts = parts.map { case Literal(str) => str.reverse }
        val strArgs = args1.map { case Literal(str) => str }
        StringContext(strParts: _*).s(strArgs: _*).toExpr
      case _ => ???
    }

  }

}
