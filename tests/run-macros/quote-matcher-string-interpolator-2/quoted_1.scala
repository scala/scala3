import scala.quoted._



object Macros {

  extension (inline self: StringContext) inline def xyz(inline args: String*): String = ${impl('self, 'args)}

  private def impl(self: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[String] = {
    (self, args) match {
      case ('{ StringContext(${Varargs(parts)}: _*) }, Varargs(args1)) =>
        val strParts = parts.map(_.valueOrError.reverse)
        val strArgs = args1.map(_.valueOrError)
        Expr(StringContext(strParts: _*).s(strArgs: _*))
      case _ => ???
    }

  }

}
