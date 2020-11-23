import scala.quoted._


inline def rewrite[T](inline x: Any): Any = ${ stringRewriter('x) }

private def stringRewriter(e: Expr[Any])(using Quotes): Expr[Any] =
  StringRewriter.transform(e)

private object StringRewriter extends ExprMap {

  def transform[T](e: Expr[T])(using Quotes, Type[T]): Expr[T] = e match
    case Const(s: String) =>
      Expr(s.reverse) match
        case '{ $x: T } => x
        case _ => e // e had a singlton String type
    case _ => transformChildren(e)

}
