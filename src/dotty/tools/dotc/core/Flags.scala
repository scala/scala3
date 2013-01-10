package dotty.tools.dotc.core

object Flags {

  /** A FlagSet represents a set of flags. Flags are encoded as follows:
   *  The first two bits indicate whether a flagset applies to terms,
   *  to types, or to both.  Bits 2..63 are available for properties
   *  and can be doubly used for terms and types.
   *  Combining two FlagSets with `|` will give a FlagSet
   *  that has the intersection of the applicability to terms/types
   *  of the two flag sets. It is checked that the intersection is not empty.
   */
  case class FlagSet(val bits: Long) extends AnyVal {

    def | (that: FlagSet) =
      if (bits == 0) that
      else if (that.bits == 0) this
      else {
        val tbits = bits & that.bits & TYPEFLAGS
        assert(tbits != 0, s"illegal flagset combination: $this and $that")
        FlagSet(tbits | ((this.bits | that.bits) & ~TYPEFLAGS))
      }

    def & (that: FlagSet) = FlagSet(bits & that.bits)

    def hasFlagIn(flags: FlagSet) = {
      val fs = bits & flags.bits
      (fs & TYPEFLAGS) != 0 &&
      fs > TYPEFLAGS
    }

    def hasFlagIn(flags: FlagSet, butNotIn: FlagSet) = {
      val fs = bits & flags.bits
      (fs & TYPEFLAGS) != 0 &&
      fs > TYPEFLAGS &&
      (bits & butNotIn.bits) == 0
    }

    def hasAllFlags(flags: FlagSet) = {
      val fs = bits & flags.bits
      (fs & TYPEFLAGS) != 0 &&
      (fs >> TYPESHIFT) == (flags.bits >> TYPESHIFT)
    }

    def hasAllFlags(flags: FlagSet, butNotIn: FlagSet) = {
      val fs = bits & (flags.bits | butNotIn.bits)
      (fs & TYPEFLAGS) != 0 &&
      (fs >> TYPESHIFT) == (flags.bits >> TYPESHIFT)
    }

    /** The set of all non-empty strings that are associated
     *  as term or type flags with this index
     */
    private def flagString(idx: Int): Set[String] =
      kindIndices.map(flagName(idx)).filterNot(_.isEmpty)

    override def toString =
      (2 to MaxFlag).flatMap(flagString).mkString(" ")
  }

  final val TYPEFLAGS = 3L
  final val TYPESHIFT = 2
  final val TERMindex = 0
  final val TYPEindex = 1
  final val TERMS = 1 << TERMindex
  final val TYPES = 1 << TYPEindex

  final val MaxFlag = 63

  private var flagName = Array.fill(64, 2)("")

  private val kindIndices = Set(TERMindex, TYPEindex)

  /** The flag with given index between 2 and 63 which applies to terms */
  def termFlag(index: Int, name: String): FlagSet = {
    flagName(index)(TERMindex) = name
    FlagSet(TERMS | (1L << index))
  }

  /** The flag with given index between 2 and 63 which applies to types */
  def typeFlag(index: Int, name: String): FlagSet = {
    flagName(index)(TYPEindex) = name
    FlagSet(TYPES | (1L << index))
  }

  /** The flag with given index between 2 and 63 which applies to both terms and types */
  def commonFlag(index: Int, name: String): FlagSet =
    termFlag(index, name) | typeFlag(index, name)

  // Available flags:

  final val Empty = FlagSet(0)

  final val Private = commonFlag(3, "private")
  final val Accessor = termFlag(4, "<accessor>")

  final val Error = FlagSet(1 << 32)
  final val Frozen = FlagSet(???)
  final val Package = FlagSet(???)

}