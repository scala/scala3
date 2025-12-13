package dotc.core

/**
 * Cross-platform flag representation for the browser compiler.
 *
 * Flags are represented as bit sets that can apply to terms, types, or both.
 */
object Flags {

  /** A FlagSet represents a set of flags encoded as a Long */
  opaque type FlagSet = Long

  object FlagSet {
    def apply(bits: Long): FlagSet = bits
    val Empty: FlagSet = 0L
  }

  /** A Flag is a single flag (a FlagSet with one bit set) */
  opaque type Flag <: FlagSet = Long

  private def Flag(index: Int, isTermFlag: Boolean, isTypeFlag: Boolean): Flag = {
    val kindBits = (if (isTermFlag) TERMS else 0L) | (if (isTypeFlag) TYPES else 0L)
    kindBits | (1L << (index + TYPESHIFT))
  }

  private def commonFlag(index: Int): Flag = Flag(index, true, true)
  private def termFlag(index: Int): Flag = Flag(index, true, false)
  private def typeFlag(index: Int): Flag = Flag(index, false, true)

  // Kind bits (bits 0-1)
  private val TYPESHIFT = 2
  private val TERMS = 1L << 0
  private val TYPES = 1L << 1
  private val KINDFLAGS = TERMS | TYPES

  extension (x: FlagSet) {
    def bits: Long = x

    /** Union of flag sets */
    def | (y: FlagSet): FlagSet = {
      if (x == 0L) y
      else if (y == 0L) x
      else {
        val tbits = x & y & KINDFLAGS
        FlagSet(tbits | ((x | y) & ~KINDFLAGS))
      }
    }

    /** Intersection of flag sets */
    def & (y: FlagSet): FlagSet = FlagSet(x & y)

    /** Intersection with complement */
    def &~ (y: FlagSet): FlagSet = {
      val tbits = x & KINDFLAGS
      if ((tbits & y) == 0L) x
      else FlagSet(tbits | ((x & ~y) & ~KINDFLAGS))
    }

    /** Check if flag is set */
    def is(flag: Flag): Boolean = {
      val fs = x & flag
      (fs & KINDFLAGS) != 0L && (fs & ~KINDFLAGS) != 0L
    }

    /** Check if flag is set but not butNot */
    def is(flag: Flag, butNot: FlagSet): Boolean = x.is(flag) && !x.isOneOf(butNot)

    /** Check if any flag in set is present */
    def isOneOf(flags: FlagSet): Boolean = {
      val fs = x & flags
      (fs & KINDFLAGS) != 0L && (fs & ~KINDFLAGS) != 0L
    }

    /** Check if all flags in set are present */
    def isAllOf(flags: FlagSet): Boolean = {
      val fs = x & flags
      ((fs & KINDFLAGS) != 0L || flags == 0L) &&
      (fs >>> TYPESHIFT) == (flags >>> TYPESHIFT)
    }

    def isEmpty: Boolean = (x & ~KINDFLAGS) == 0L

    def flagsString: String = {
      val sb = new StringBuilder
      var remaining = x & ~KINDFLAGS
      var bit = TYPESHIFT
      while (remaining != 0L) {
        if ((remaining & 1L) != 0L) {
          if (sb.nonEmpty) sb.append(" | ")
          sb.append(flagName(bit))
        }
        remaining >>>= 1
        bit += 1
      }
      if (sb.isEmpty) "<none>" else sb.toString
    }
  }

  // Flag definitions
  private var nextBit = 0
  private def nextFlag(term: Boolean, tpe: Boolean): Flag = {
    val f = Flag(nextBit, term, tpe)
    nextBit += 1
    f
  }

  // Common flags (apply to both terms and types)
  val Private: Flag = nextFlag(true, true)       // 0
  val Protected: Flag = nextFlag(true, true)     // 1
  val Abstract: Flag = nextFlag(true, true)      // 2
  val Final: Flag = nextFlag(true, true)         // 3
  val Sealed: Flag = nextFlag(false, true)       // 4
  val Case: Flag = nextFlag(true, true)          // 5
  val Implicit: Flag = nextFlag(true, true)      // 6
  val Given: Flag = nextFlag(true, true)         // 7
  val Erased: Flag = nextFlag(true, true)        // 8
  val Lazy: Flag = nextFlag(true, false)         // 9
  val Override: Flag = nextFlag(true, false)     // 10
  val Inline: Flag = nextFlag(true, true)        // 11
  val Macro: Flag = nextFlag(true, false)        // 12
  val Static: Flag = nextFlag(true, false)       // 13
  val Object: Flag = nextFlag(true, false)       // 14 (Module)
  val Trait: Flag = nextFlag(false, true)        // 15
  val Enum: Flag = nextFlag(true, true)          // 16
  val Local: Flag = nextFlag(true, true)         // 17
  val Synthetic: Flag = nextFlag(true, true)     // 18
  val Artifact: Flag = nextFlag(true, true)      // 19
  val Mutable: Flag = nextFlag(true, false)      // 20
  val Accessor: Flag = nextFlag(true, false)     // 21
  val CaseAccessor: Flag = nextFlag(true, false) // 22
  val Covariant: Flag = nextFlag(false, true)    // 23
  val Contravariant: Flag = nextFlag(false, true) // 24
  val Param: Flag = nextFlag(true, true)         // 25
  val ParamAccessor: Flag = nextFlag(true, false) // 26
  val Package: Flag = nextFlag(true, true)       // 27
  val Method: Flag = nextFlag(true, false)       // 28
  val Deferred: Flag = nextFlag(true, true)      // 29
  val Open: Flag = nextFlag(false, true)         // 30
  val Transparent: Flag = nextFlag(true, true)   // 31
  val Infix: Flag = nextFlag(true, false)        // 32
  val Extension: Flag = nextFlag(true, false)    // 33
  val Opaque: Flag = nextFlag(false, true)       // 34
  val Exported: Flag = nextFlag(true, false)     // 35
  val Using: Flag = nextFlag(true, false)        // 36
  val Stable: Flag = nextFlag(true, false)       // 37

  // Aliases
  val Module: Flag = Object
  val AbsOverride: FlagSet = Abstract | Override

  // Common flag sets
  val EmptyFlags: FlagSet = FlagSet.Empty
  val AccessFlags: FlagSet = Private | Protected | Local
  val ModifierFlags: FlagSet = Private | Protected | Abstract | Final | Sealed |
    Case | Implicit | Given | Lazy | Override | Inline | Transparent | Infix | Open | Opaque
  val CommonSourceModifierFlags: FlagSet = Private | Protected | Final | Case | Implicit | Given | Override
  val TermSourceModifierFlags: FlagSet = CommonSourceModifierFlags | Lazy | Inline | Transparent | Infix
  val TypeSourceModifierFlags: FlagSet = CommonSourceModifierFlags | Sealed | Open | Opaque

  private def flagName(bit: Int): String = bit match {
    case 2 => "private"
    case 3 => "protected"
    case 4 => "abstract"
    case 5 => "final"
    case 6 => "sealed"
    case 7 => "case"
    case 8 => "implicit"
    case 9 => "given"
    case 10 => "erased"
    case 11 => "lazy"
    case 12 => "override"
    case 13 => "inline"
    case 14 => "macro"
    case 15 => "static"
    case 16 => "object"
    case 17 => "trait"
    case 18 => "enum"
    case 19 => "local"
    case 20 => "synthetic"
    case 21 => "artifact"
    case 22 => "mutable"
    case 23 => "accessor"
    case 24 => "caseAccessor"
    case 25 => "covariant"
    case 26 => "contravariant"
    case 27 => "param"
    case 28 => "paramAccessor"
    case 29 => "package"
    case 30 => "method"
    case 31 => "deferred"
    case 32 => "open"
    case 33 => "transparent"
    case 34 => "infix"
    case 35 => "extension"
    case 36 => "opaque"
    case 37 => "exported"
    case 38 => "using"
    case 39 => "stable"
    case n => s"bit$n"
  }
}

