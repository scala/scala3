sealed trait DFTypeAny

sealed trait DFTuple[T <: NonEmptyTuple] extends DFTypeAny

sealed trait DFBit extends DFTypeAny

sealed trait DFValOf[T]

type Of[T] <: DFTypeAny = T match
  case DFTypeAny => T & DFTypeAny
  case Product   => FromProduct[T]

type JUSTVAL[T] = DFValOf[Of[T]]

type FromProduct[T <: Product] <: DFTypeAny = T match
  case NonEmptyTuple => DFTuple[Tuple.Map[T, JUSTVAL]]

trait Width2[T]

object Width2:
  inline given [T]: Width2[T] = new Width2[T] {}

val x = summon[Width2[Of[(DFBit, DFBit)]]]
