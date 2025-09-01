import compiletime._
import compiletime.ops.int._

object Test {
  type TupleTypeIndex[T <: Tuple, C] <: Int = T match {
    case C *: t => 0
    case h *: t => S[TupleTypeIndex[t, C]]
  }

  trait TupleExtractor[TT <: Tuple, C] {
    def get(t: TT): C
  }

  given [T <: Tuple, C, EV <: TupleTypeIndex[T, C]] => TupleExtractor[T, C]:
    def get(t: T): C = t.toArray.apply(toIntC[TupleTypeIndex[T, C]]).asInstanceOf[C] // error

  transparent inline def toIntC[N <: Int]: Int =
    inline constValue[N] match
      case 0        => 0
      case _: S[n1] => 1 + toIntC[n1]

}
