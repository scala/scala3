import scala.quoted._


object Macros {

  inline def (inline self: StringContext) xyz(args: => String*): String = ${impl('self, 'args)}

  private def impl(self: Expr[StringContext], args: Expr[Seq[String]])(using QuoteContext): Expr[String] = {
    self match {
      case '{ StringContext($parts: _*) } =>
        '{ StringContext($parts: _*).s($args: _*) }
      case _ =>
        '{ "ERROR" }
    }
  }
}
