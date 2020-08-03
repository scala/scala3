import scala.quoted._


inline def rewrite[T](inline x: Any): Any = ${ stringRewriter('x) }

private def stringRewriter(e: Expr[Any])(using QuoteContext): Expr[Any] =
  StringRewriter.transform(e)

private object StringRewriter extends util.ExprMap {

  def transform[T](e: Expr[T])(using QuoteContext, Staged[T]): Expr[T] = e match
    case '{ ($x: Foo).x } =>
      '{ new Foo(4).x } match
        case '{ $e: T } => e
    case _ =>
      transformChildren(e)

}

case class Foo(x: Int)
