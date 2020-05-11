import scala.quoted._

type G[X]
case class Foo[T](x: T)
def f(using s: Scope)(word: String): s.Expr[Foo[G[String]]] = '{Foo(${Expr(word)})} // error
