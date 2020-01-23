import scala.quoted._
import scala.quoted.matching._

object Macros {

  inline def (self: => StringContext) xyz(args: => String*): String = ${impl('self, 'args)}

  private def impl(self: Expr[StringContext], args: Expr[Seq[String]]) with QuoteContext : Expr[String] = {
    self match {
      case '{ StringContext($parts: _*) } =>
        '{ StringContext($parts: _*).s($args: _*) }
      case _ =>
        '{ "ERROR" }
    }
  }
}
