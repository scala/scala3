import scala.compiletime.*

trait C[A]

inline given [Tup <: Tuple]: C[Tup] with // error
  val cs = summonAll[Tuple.Map[Tup, C]]
