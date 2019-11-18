import scala.quoted._
import scala.quoted.matching._

inline def rewrite[T](x: => Any): Any = ${ stringRewriter('x) }

private def stringRewriter(e: Expr[Any])(given QuoteContext): Expr[Any] =
  StringRewriter.map(e)

private object StringRewriter extends util.ExprMap {

  def map[T](e: Expr[T])(given QuoteContext, Type[T]): Expr[T] = e match
    case Const(s: String) =>
      Expr(s.reverse) match
        case '{ $x: T } => x
        case _ => e // e had a singlton String type
    case _ => mapChildren(e)

}
