import scala.quoted.*

object Foo {

  def f(e: Expr[Any])(using Quotes): Unit = e match {
    case '{ foo[t]($x) } => bar(x)
    case '{ foo[t]($x) } if bar(x) => ()
    case '{ foo[t]($x) } => '{ foo($x) }
    case '{ foo[t]($x) } if bar[Any]('{ foo($x) }) => ()
  }

  def foo[T](t: T): Unit = ()

  def bar[T: Type](t: Expr[T]): Boolean = true

}
