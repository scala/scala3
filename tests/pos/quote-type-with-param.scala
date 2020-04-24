import scala.quoted._

def f(using QuoteContext): Unit =
  '{ type T[X] = List[X] }
