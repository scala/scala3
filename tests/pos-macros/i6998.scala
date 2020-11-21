import scala.quoted._

def foo(using Quotes) : Unit = {
  val '{ $f : (Int => Double) } = ??? : Expr[Any]
}
