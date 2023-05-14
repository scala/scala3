import scala.compiletime.*

trait C[A]

inline given [Tup <: Tuple]: C[Tup] with
  val cs = summonAll[Tuple.Map[Tup, C]] // error cannot reduce inline match with
