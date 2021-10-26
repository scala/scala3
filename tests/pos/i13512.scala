import scala.language.strictEquality

class NotEquatable

def f = List(new NotEquatable) match
  case Nil => ???
  case _ =>
