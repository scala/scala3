import scala.quoted._

def foo with QuoteContext : Unit = {
  val '{ $f : (Int => Double) } = ???
}
