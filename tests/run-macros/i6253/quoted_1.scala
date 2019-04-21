import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection

object Macros {

  inline def (self: => StringContext) xyz(args: => String*): String = ${impl('self, 'args)}

  private def impl(self: Expr[StringContext], args: Expr[Seq[String]])(implicit reflect: Reflection): Expr[String] = {
    self match {
      case '{ StringContext($parts: _*) } =>
        '{ StringContext($parts: _*).s($args: _*) }
      case _ =>
        '{ "ERROR" }
    }
  }
}
