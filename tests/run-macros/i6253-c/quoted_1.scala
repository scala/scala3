import scala.quoted.*


object Macros {

  // Should be: extension (inline self: StringContext) inline def ...
  extension (self: => StringContext) inline def xyz(args: => String*): String = ${impl('self, 'args)}

  private def impl(self: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[String] = {
    self match {
      case '{ StringContext($parts*) } => // Should not match as the parameter is not marked as inlined
        '{ ??? }
      case _ =>
        '{ "Ok" }
    }
  }
}
