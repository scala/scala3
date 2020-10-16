sealed trait Datatype[T]
case class Foo[T <: Tuple](elems: Tuple.Map[T, Datatype]) extends Datatype[T]

def g =
  val tt: Datatype[_] = ???
  tt match {
    case Foo(_) => ???
  }
