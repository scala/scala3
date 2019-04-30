import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection

object Macros {

  inline def (self: => StringContext) xyz(args: => String*): String = ${impl('self, 'args)}

  private def impl(self: Expr[StringContext], args: Expr[Seq[String]])(implicit reflect: Reflection): Expr[String] = {
    (self, args) match {
      case ('{ StringContext(${ExprSeq(parts)}: _*) }, ExprSeq(args1)) =>
        val strParts = parts.map { case Const(str) => str.reverse }
        val strArgs = args1.map { case Const(str) => str }
        StringContext(strParts: _*).s(strArgs: _*).toExpr
      case _ => ???
    }

  }

}
