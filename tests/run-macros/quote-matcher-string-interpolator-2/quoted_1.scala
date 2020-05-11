import scala.quoted._



object Macros {

  extension (inline self: StringContext) inline def xyz(inline args: String*): String = ${impl('self, 'args)}

  private def impl(using s: Scope)(self: s.Expr[StringContext], args: s.Expr[Seq[String]]): s.Expr[String] = {
    (self, args) match {
      case ('{ StringContext(${Varargs(parts)}: _*) }, Varargs(args1)) =>
        val strParts = parts.map { case Const(str) => str.reverse }
        val strArgs = args1.map { case Const(str) => str }
        Expr(StringContext(strParts: _*).s(strArgs: _*))
      case _ => ???
    }

  }

}
