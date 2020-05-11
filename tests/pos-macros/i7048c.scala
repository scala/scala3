import scala.quoted._

trait IsExpr {
  type Underlying
}

val foo: IsExpr = ???

def g(using s: Scope)(e: IsExpr)(using tu: s.Type[e.Underlying]): Unit = ???

def mcrImpl(using s: Scope): Unit = {
  g(foo)
}
