package dotty.tools.benchmarks.inlinetraits
package inlinetrait

inline trait Pair[+T1, +T2](val _1: T1, val _2: T2)

class IntDoublePair(override val _1: Int, override val _2: Double) extends Pair[Int, Double](_1, _2)
class CharShortPair(override val _1: Char, override val _2: Short) extends Pair[Char, Short](_1, _2)