import scala.quoted._

def foo(using QuoteContext) : Unit = {
  val '{ $f : (Int => Double) } = ???
}
