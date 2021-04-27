import scala.quoted.*


object Macros {

  extension (inline self: StringContext) inline def xyz(args: => String*): String = ${impl('self, 'args)}

  private def impl(self: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[String] = {
    self match {
      case '{ StringContext($parts*) } =>
        '{ StringContext($parts*).s($args*) }
      case _ =>
        '{ "ERROR" }
    }
  }
}
