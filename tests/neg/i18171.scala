type BAZ[T] = T match
  case Any => DFVal[BAZREC[T]]

type BAZREC[T] = T match
  case NonEmptyTuple => Tuple.Map[T, BAZ]

trait DFVal[T]

def foo(relIdx: BAZ[Any]): Unit =
  relIdx.bar // error
