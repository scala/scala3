import scala.deriving._

sealed trait Decoder[T]
object Decoder {
  given Decoder[Double] = ???

  inline given summonEmptyTuple[H]: Tuple.Map[EmptyTuple.type, Decoder] =
    EmptyTuple

  inline given summonTuple[H, T <: Tuple](using hd: Decoder[H], td: Tuple.Map[T, Decoder]): Tuple.Map[H *: T, Decoder] =
    hd *: td

  inline given derived[T](using m: Mirror.Of[T], d: Tuple.Map[m.MirroredElemTypes, Decoder]): Decoder[T] = ???
}
