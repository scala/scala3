import scala.quoted.*

def f(using Quotes): Unit =
  '{ type T[X] = List[X] }
