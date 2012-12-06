package dotty.tools.dotc.core

object Flags {

  type FlagSet = Long

  final val Empty = 0

  final val Error = 1 << 32
  final val Frozen: Int = ???
  final val Private: Int = ???

}