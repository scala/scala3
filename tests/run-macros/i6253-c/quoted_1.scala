import scala.quoted._


object Macros {

  // Should be: extension (inline self: StringContext) inline def ...
  extension (self: => StringContext) inline def xyz(args: => String*): String = ${impl('self, 'args)}

  private def impl(using s: Scope)(self: s.Expr[StringContext], args: s.Expr[Seq[String]]): s.Expr[String] = {
    self match {
      case '{ StringContext($parts: _*) } => // Should not match as the parameter is not marked as inlined
        '{ ??? }
      case _ =>
        '{ "Ok" }
    }
  }
}
