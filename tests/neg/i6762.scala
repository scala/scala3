import scala.quoted.{_, given}

type G[X]
case class Foo[T](x: T)
def f(word: String)(given QuoteContext): Expr[Foo[G[String]]] = '{Foo(${word.toExpr})} // error
