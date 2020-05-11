import scala.quoted._


object Macros {

  extension (inline self: StringContext) inline def xyz(args: => String*): String = ${impl('self, 'args)}

  private def impl(using s: Scope)(self: s.Expr[StringContext], args: s.Expr[Seq[String]]): s.Expr[String] = {
    self match {
      case '{ StringContext($parts: _*) } =>
        '{ StringContext($parts: _*).s($args: _*) }
      case _ =>
        '{ "ERROR" }
    }
  }
}
