sealed trait Typed[T]
case class Bool() extends Typed[Boolean]
case class Str() extends Typed[String]
case class Num() extends Typed[Int]

type Value[T] = T match
  case Boolean => (true, false)
  case String => List[Char]
  case Num => Unit

def foo[T](typed: Typed[T]) = typed match
  case Bool() => ??? : Value[T]
  case Str()  => ??? : Value[T]
  case Num()  => ??? : Value[T]
