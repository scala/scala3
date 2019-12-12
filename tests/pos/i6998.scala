import scala.quoted._

def foo(given QuoteContext) : Unit = {
  val '{ $f : (Int => Double) } = ???
}
