import scala.compiletime.*

trait C[A]

inline given [Tup <: Tuple] => C[Tup]:
  val cs = summonAll[Tuple.Map[Tup, C]] // error: Tuple element types must be known at compile time
