import scala.quoted._

trait IsExpr {
  type Underlying
}

val foo: IsExpr = ???

def g(e: IsExpr) with (tu: Type[e.Underlying]) : Unit = ???

def mcrImpl with QuoteContext : Unit = {
  g(foo)
}
