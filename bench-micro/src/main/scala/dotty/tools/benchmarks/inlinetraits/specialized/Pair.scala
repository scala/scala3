package dotty.tools.benchmarks.inlinetraits
package specialized

import scala.specialized

case class Pair[@specialized(Int, Char) +T1, @specialized(Double, Short) +T2](_1: T1, _2: T2) {
  final def toTuple: (T1, T2) = (_1, _2)
}
