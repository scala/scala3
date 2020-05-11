import scala.quoted._


inline def rewrite[T](inline x: Any): Any = ${ stringRewriter('x) }

private def stringRewriter(using s: Scope)(e: s.Expr[Any]): s.Expr[Any] =
  StringRewriter.transform(e)

private object StringRewriter extends util.ExprMap {

  def transform[T](using s: Scope)(e: s.Expr[T])(using s.Type[T]): s.Expr[T] = e match
    case '{ ($x: Foo).x } =>
      '{ new Foo(4).x } match
        case '{ $e: T } => e
    case _ =>
      transformChildren(e)

}

case class Foo(x: Int)
