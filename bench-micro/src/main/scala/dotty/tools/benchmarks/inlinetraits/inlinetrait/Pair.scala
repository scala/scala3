package dotty.tools.benchmarks.inlinetraits
package inlinetrait

inline trait Pair[+T1, +T2]:
  val _1: T1
  val _2: T2
  final def toTuple: (T1, T2) = (_1, _2)

case class IntDoublePair(_1: Int, _2: Double) extends Pair[Int, Double]
case class CharShortPair(_1: Char, _2: Short) extends Pair[Char, Short]