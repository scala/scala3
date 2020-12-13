import scala.quoted._



object Macros {

  extension (inline self: StringContext) inline def S(args: => String*): String = ${impl('self, 'args)}

  private def impl(self: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[String] = {
    self match {
      case '{ StringContext(${Varargs(Consts(parts))}: _*) } =>
        val upprerParts: List[String] = parts.toList.map(_.toUpperCase)
        val upprerPartsExpr: Expr[List[String]] = Expr.ofList(upprerParts.map(Expr(_)))
        '{ StringContext($upprerPartsExpr: _*).s($args: _*) }
      case _ =>
        '{
          val parts: Seq[String] = $self.parts
          val upprerParts = parts.map(_.toUpperCase)
          StringContext(upprerParts: _*).s($args: _*)
        }
    }

  }

}
