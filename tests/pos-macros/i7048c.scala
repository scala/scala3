import scala.quoted.*

trait IsExpr {
  type Underlying
}

val foo: IsExpr = ???

def g(e: IsExpr)(using tu: Type[e.Underlying]): Unit = ???

def mcrImpl(using Quotes): Unit = {
  g(foo)
}
