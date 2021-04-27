import scala.quoted.*


inline def rewrite[T](inline x: Any): Any = ${ stringRewriter('x) }

private def stringRewriter(e: Expr[Any])(using Quotes): Expr[Any] =
  StringRewriter.transform(e)

private object StringRewriter extends ExprMap {

  def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] = e match
    case '{ ${Expr(s)}: String } =>
      // checkIfValid(s)
      val s2: String & T = s
      Expr(s2)
    case _ => transformChildren(e)

}
