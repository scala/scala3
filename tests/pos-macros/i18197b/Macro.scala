import scala.quoted.*

object ReproMacro {
  inline def readPath[A, B](inline config: Config[A, B]) = ${ readPathMacro[A, B]('config) }

  def readPathMacro[A: Type, B: Type](expr: Expr[Config[A, B]])(using Quotes) = {
    import quotes.reflect.report

    expr match {
      case '{ Field.const[a, b, tpe]($selector) } =>
        val selector2: Expr[Selector ?=> a => tpe] = selector
        report.info(s"Matched!")
        '{}
      case other =>
        report.errorAndAbort("woops, I did not match")
    }
  }
}
