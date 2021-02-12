import scala.quoted.*


object Macros {

  extension (inline self: StringContext) inline def xyz(args: => String*): String = ${impl('self, 'args)}

  private def impl(self: Expr[StringContext], args: Expr[Seq[String]])(using Quotes): Expr[String] = {
    self match {
      case '{ StringContext($parts*) } =>
        '{
          val p: Seq[String] = $parts
          val a: Seq[Any] = $args ++ Seq("")
          p.zip(a).map(_ + _.toString).mkString
        }
      case _ =>
        '{ "ERROR" }
    }
  }
}
