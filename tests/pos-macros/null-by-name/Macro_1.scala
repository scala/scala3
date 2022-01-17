import scala.quoted.*

inline def foo(x: => Any): Unit =
  ${ impl('x) }

private def impl(x: Expr[Any])(using Quotes) : Expr[Unit] = {
  '{
    val a = $x
  }
}
