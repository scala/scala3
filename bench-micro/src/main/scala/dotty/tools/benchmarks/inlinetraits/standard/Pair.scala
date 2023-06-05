package dotty.tools.benchmarks.inlinetraits
package standard

case class Pair[+T1, +T2](_1: T1, _2: T2):
  final def toTuple: (T1, T2) = (_1, _2)
