import scala.quoted.{_, given _}

type G[X]
case class Foo[T](x: T)
def f(word: String) with QuoteContext : Expr[Foo[G[String]]] = '{Foo(${Expr(word)})} // error // error
