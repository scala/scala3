import scala.quoted.*


inline def rewrite[T](inline x: Any): Any = ${ stringRewriter('x) }

private def stringRewriter(e: Expr[Any])(using Quotes): Expr[Any] =
  StringRewriter.transform(e)

private object StringRewriter extends ExprMap {

  def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] = e match
    case '{ ($x: Foo).x } =>
      '{ new Foo(4).x } match
        case '{ $e: T } => e
    case _ =>
      transformChildren(e)

}

case class Foo(x: Int)
