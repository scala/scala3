import scala.quoted._


object Macros {

  // Should be: inline def (inline self: StringContext) ...
  inline def (self: => StringContext) xyz(args: => String*): String = ${impl('self, 'args)}

  private def impl(self: Expr[StringContext], args: Expr[Seq[String]])(using QuoteContext): Expr[String] = {
    self match {
      case '{ StringContext($parts: _*) } => // Should not match as the parameter is not marked as inlined
        '{ ??? }
      case _ =>
        '{ "Ok" }
    }
  }
}
