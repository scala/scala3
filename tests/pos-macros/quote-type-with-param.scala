import scala.quoted._

def f(using Quotes): Unit =
  '{ type T[X] = List[X] }
