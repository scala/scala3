import scala.quoted._

object Foo {

  def f(using s: Scope)(e: s.Expr[Any]): Unit = e match {
    case '{ foo[$t]($x) } => bar(x)
    case '{ foo[$t]($x) } if bar(x) => ()
    case '{ foo[$t]($x) } => '{ foo($x) }
    case '{ foo[$t]($x) } if bar[Any]('{ foo($x) }) => ()
  }

  def foo[T](t: T): Unit = ()

  def bar[T](using s: Scope)(t: s.Expr[T])(using s.Type[T]): Boolean = true

}
