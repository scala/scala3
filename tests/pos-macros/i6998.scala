import scala.quoted._

def foo(using s: Scope) : Unit = {
  val '{ $f : (Int => Double) } = ??? : s.Expr[Any]
}
