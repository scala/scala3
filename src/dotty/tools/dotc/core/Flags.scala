package dotty.tools.dotc.core

object Flags {

  case class FlagSet(val bits: Long) extends AnyVal {
    def | (that: FlagSet) = FlagSet(this.bits | that.bits)
    def & (that: FlagSet) = FlagSet(this.bits & that.bits)
  }

  final val Empty = FlagSet(0)

  final val Error = FlagSet(1 << 32)
  final val Frozen = FlagSet(???)
  final val Private = FlagSet(???)
  final val Package = FlagSet(???)

}