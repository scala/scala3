import scala.quoted.*

type G[X]
case class Foo[T](x: T)
def f(word: String)(using Quotes): Expr[Foo[G[String]]] = '{Foo(${Expr(word)})} // error // error
