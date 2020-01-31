import scala.quoted._

trait IsExpr {
  class Underlying
}

val foo: IsExpr = ???

def g(e: IsExpr)(given tu: Type[e.Underlying]): Unit = ???

def mcrImpl(given QuoteContext): Unit = {
  g(foo)
}
