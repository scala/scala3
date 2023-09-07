import scala.quoted._

def f(foo: Any => Any)(using Quotes): Expr[Any] =
  '{ println(${ foo[Int]('{???}); ??? }) } // error
