import scala.quoted.*

def foo(using Quotes) : Unit = {
  val '{ $f : (Int => Double) } = ??? : Expr[Any]
}
