import scala.Tuple as STuple

enum Datatype[T] {
  case Tuple[T <: STuple](elems: STuple.Map[T, Datatype]) extends Datatype[T]
}

object Datatype {
  given [H, T <: STuple] => (ht: Datatype[H], tt: Datatype[T]) => Datatype[H *: T] = tt match {
    case Datatype.Tuple(elems) => Datatype.Tuple(ht *: elems)
  }
}
