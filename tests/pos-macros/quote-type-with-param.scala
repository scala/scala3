import scala.quoted._

def f(using s: Scope): Unit =
  '{ type T[X] = List[X] }
