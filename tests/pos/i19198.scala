import deriving.Mirror
import compiletime.summonInline

inline def check1[Tps <: NonEmptyTuple]: Unit =
  summonInline[Mirror.Of[Tuple.Head[Tps]]]

inline def check2[Tps <: NonEmptyTuple]: Unit =
  type FromType = Tuple.Head[Tps]
  summonInline[Mirror.Of[FromType]]

@main def Test: Unit =
  check1[Option[Int] *: EmptyTuple] // Ok
  check2[Option[Int] *: EmptyTuple] // Error: FromType is widened to Any in Syntheziser
