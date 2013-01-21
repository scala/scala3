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

    /** The union of this flag set and the given flag set
     */
    def | (that: FlagSet): FlagSet =
      if (bits == 0) that
      else if (that.bits == 0) this
      else {
        val tbits = bits & that.bits & KINDFLAGS
        assert(tbits != 0, s"illegal flagset combination: $this and $that")
        FlagSet(tbits | ((this.bits | that.bits) & ~KINDFLAGS))
      }

    /** The union of this flag set and the given flag conjunction seen as
     *  a flag set.
     */
    def | (that: FlagConjunction): FlagSet = this | FlagSet(that.bits)

    /** The intersection of this flag set and the given flag set */
    def & (that: FlagSet) = FlagSet(bits & that.bits)

    /** The intersection of this flag set with the complement of the given flag set */
    def &~ (that: FlagSet) = {
      val tbits = bits & KINDFLAGS
      if ((tbits & that.bits) == 0) this
      else FlagSet(tbits | ((this.bits & ~that.bits) & ~KINDFLAGS))
    }

    /** Does this flag set have a non-empty intersection with the given flag set?
     *  Pre: The intersection of the typeflags of both sets must be non-empty.
     */
    def is(flags: FlagSet) = {
      val fs = bits & flags.bits
      (fs & KINDFLAGS) != 0 &&
      fs > KINDFLAGS
    }

   /** Does this flag set have a non-empty intersection with the given flag set,
    *  and at the same time contain none of the flags in the `butNot` set?
    *  Pre: The intersection of the typeflags of both sets must be non-empty.
    */
    def is(flags: FlagSet, butNot: FlagSet) = {
      val fs = bits & flags.bits
      (fs & KINDFLAGS) != 0 &&
      fs > KINDFLAGS &&
      (bits & butNot.bits) == 0
    }

    /** Does this flag set have all of the flags in given flag conjunction?
     *  Pre: The intersection of the typeflags of both sets must be non-empty.
     */
    def is(flags: FlagConjunction) = {
      val fs = bits & flags.bits
      (fs & KINDFLAGS) != 0 &&
      (fs >> TYPESHIFT) == (flags.bits >> TYPESHIFT)
    }

    /** Does this flag set have all of the flags in given flag conjunction?
     *  and at the same time contain none of the flags in the `butNot` set?
     *  Pre: The intersection of the typeflags of both sets must be non-empty.
     */
    def is(flags: FlagConjunction, butNot: FlagSet) = {
      val fs = bits & (flags.bits | butNot.bits)
      (fs & KINDFLAGS) != 0 &&
      (fs >> TYPESHIFT) == (flags.bits >> TYPESHIFT)
    }

    /** This flag set with all flags transposed to be type flags */
    def toTypeFlags = FlagSet(bits & ~KINDFLAGS | TYPES)

    /** This flag set with all flags transposed to be term flags */
    def toTermFlags = FlagSet(bits & ~KINDFLAGS | TERMS)

    /** This flag set with all flags transposed to be common flags */
    def toCommonFlags = FlagSet(bits | KINDFLAGS)

    /** The number of non-kind flags in this set */
    def numFlags: Int = java.lang.Long.bitCount(bits & ~KINDFLAGS)

    /** The set of all non-empty strings that are associated
     *  as term or type flags with this index
     */
    private def flagString(idx: Int): Set[String] =
      kindIndices.map(flagName(idx)).filterNot(_.isEmpty)

    /** The string representation of this flag set */
    override def toString =
      (2 to MaxFlag).flatMap(flagString).mkString(" ")
  }

  /** A class representing flag sets that should be tested
   *  conjunctively. I.e. for a flag conjunction `fc`,
   *  `x is fc` tests whether `x` contains all flags in `fc`.
   */
  case class FlagConjunction(bits: Long)

  private final val TYPESHIFT = 2
  private final val TERMindex = 0
  private final val TYPEindex = 1
  private final val TERMS = 1 << TERMindex
  private final val TYPES = 1 << TYPEindex
  private final val KINDFLAGS = TERMS | TYPES

  private final val MaxFlag = 63

  private var flagName = Array.fill(64, 2)("")

  private val kindIndices = Set(TERMindex, TYPEindex)

  /** The flag with given index between 2 and 63 which applies to terms.
   *  Installs given name as the name of the flag. */
  def termFlag(index: Int, name: String): FlagSet = {
    flagName(index)(TERMindex) = name
    FlagSet(TERMS | (1L << index))
  }

   /** The flag with given index between 2 and 63 which applies to types.
   *  Installs given name as the name of the flag. */
  def typeFlag(index: Int, name: String): FlagSet = {
    flagName(index)(TYPEindex) = name
    FlagSet(TYPES | (1L << index))
  }

  /** The flag with given index between 2 and 63 which applies to both terms and types */
  def commonFlag(index: Int, name: String): FlagSet = {
    flagName(index)(TERMindex) = name
    flagName(index)(TYPEindex) = name
    FlagSet(TERMS | TYPES | (1L << index))
  }

  /** The conjunction of all flags in given flag set */
  def allOf(flagss: FlagSet*) = {
    assert(flagss forall (_.numFlags == 1))
    FlagConjunction(oneOf(flagss: _*).bits)
  }
  /** The disjunction of all flags in given flag set */
  def oneOf(flagss: FlagSet*) = (Empty /: flagss) (_ | _)

  /** The disjunction of all flags in given flag set */
  def commonFlags(flagss: FlagSet*) = oneOf(flagss map (_.toCommonFlags): _*)

  /** The empty flag set */
  final val Empty = FlagSet(0)

  // Available flags:

  /** Labeled with `private` modifier */
  final val Private = commonFlag(2, "private")

  /** Labeled with `protected` modifier */
  final val Protected = commonFlag(3, "protected")

  /** Labeled with `override` modifier */
  final val Override = commonFlag(4, "override")

  /** A declared, but not defined member */
  final val Deferred = commonFlag(5, "<deferred>")

  /** Labeled with `final` modifier */
  final val Final = commonFlag(6, "final")

  /** A method. !!! needed? */
  final val Method = termFlag(7, "<method>")

  /** Labeled with `abstract` modifier (an abstract class) */
  final val Abstract = typeFlag(8, "abstract")

  /** A trait that has only abstract methods as members
   *  (and therefore can be represented by a Java interface
   */
  final val Interface = typeFlag(9, "interface")

  /** A value or class implementing a module */
  final val Module = commonFlag(10, "module")
  final val ModuleObj = Module.toTermFlags
  final val ModuleClass = Module.toTypeFlags

  /** Labeled with `implicit` modifier (implicit value) */
  final val Implicit = termFlag(11, "implicit")

  /** Labeled with `sealed` modifier (sealed class) */
  final val Sealed = typeFlag(12, "sealed")

  /** A case class or its companion object */
  final val Case = commonFlag(13, "case")
  final val CaseClass = Case.toTypeFlags
  final val CaseObj = Case.toTermFlags

  /** Labeled with `lazy` (a lazy val). */
  final val Lazy = termFlag(14, "lazy")

  /** A mutable var */
  final val Mutable = termFlag(14, "mutable")

  /** A (term or type) parameter to a class or method */
  final val Param     = commonFlag(15, "<param>")
  final val TermParam = Param.toTermFlags
  final val TypeParam = Param.toTypeFlags

  /** A value or class representing a package */
  final val Package = commonFlag(16, "<package>")
  final val PackageObj = Package.toTermFlags
  final val PackageClass = Package.toTypeFlags

  /** A by-name parameter !!! needed? */
  final val ByNameParam = termFlag(17, "<by-name>")

  /** A covariant type variable */
  final val Covariant = typeFlag(17, "<covariant>")

  /** Method is a label. */
  final val Label = termFlag(18, "<label>")

  /** Symbol is a macro */
  final val Macro = commonFlag(???, "<macro>")

  /** A contravariant type variable */
  final val Contravariant = typeFlag(18, "<contravariant>")

  /** Labeled with of abstract & override */
  final val AbsOverride = termFlag(19, "abstract override")

  /** Symbol is local to current class (i.e. private[this] or protected[this]
   *  pre: Private or Protected are also set
   */
  final val Local = commonFlag(20, "<local>")

  /** Symbol is defined by a Java class */
  final val Java = commonFlag(21, "<java>")

  /** A compiler-generated symbol. which is visible for type-checking
   *  (compare with artifact)
   */
  final val Synthetic = commonFlag(22, "<synthetic>")

  /** Method is assumed to be stable */
  final val Stable = termFlag(23, "<stable>")

  final val Static = commonFlag(24, "<static>")

  /** A value or variable accessor (getter or setter) */
  final val Accessor = termFlag(25, "<accessor>")

  /** A case parameter (or its accessor, or a GADT skolem) */
  final val CaseAccessor = termFlag(26, "<caseaccessor>")

  /** A super accessor */
  final val SuperAccessor = termFlag(27, "<superaccessor>")

  /** A field generated for a primary constructor parameter (no matter if it's a 'val' or not),
   *  or an accessor of such a field.
   */
  final val ParamAccessor = termFlag(28, "<paramaccessor>")

  /** A parameter with a default value */
  final val DefaultParam = termFlag(27, "<defaultparam>")

  /** A trait */
  final val Trait = typeFlag(27, "<trait>")

  /** A bridge method. Set by Erasure */
  final val Bridge = termFlag(28, "<bridge>")

  /** Symbol is initialized to the default value, e.g. var x: T = _ */
  final val DefaultInit = termFlag(29, "<defaultinit>")

  /** An error symbol */
  final val Erroneous = commonFlag(???, "<is-error>")

  /** Denotation is in train of being loaded and completed, flag to catch cyclic dependencies */
  final val Locked = commonFlag(???, "<locked>")

  /** Variable is accessed from nested function. */
  final val Captured = termFlag(???, "<captured>")

  /** Class symbol is defined in this/superclass constructor. */
  final val Inconstructor = typeFlag(???, "<in-constructor>")

  /** Class is not allowed to accept new members because fingerprint of subclass has been taken */
  final val Frozen = typeFlag(???, "<frozen>")

  /** Class has been lifted out to package level, local value has been lifted out to class level */
  final val Lifted = termFlag(???, "<lifted>")

  /** Term member has been mixed in */
  final val MixedIn = termFlag(???, "<mixedin>")

  /** Symbol is a generated specialized member */
  final val Specialized = commonFlag(???, "<specialized>")

  /** Symbol is a Java-style varargs method */
  final val JavaVarargs = termFlag(???, "<varargs>")

  /** Symbol is a Java varargs bridge */
  final val VBridge = termFlag(???, "<vbridge>")

  /** Symbol is a method which should be marked ACC_SYNCHRONIZED */
  final val Synchronized = termFlag(???, "<synchronized>")

  /** Symbol should be ignored when typechecking; will be marked ACC_SYNTHETIC in bytecode */
  final val Artifact = commonFlag(???, "<artifact>")

  /** Symbol is an implementation class */
  final val ImplClass = typeFlag(???, "<implclass>")

// --------- Combined Flag Sets and Conjunctions ----------------------

  /** Flags representing source modifiers */
  final val ModifierFlags = commonFlags(
    Private, Protected, Abstract, Final, Sealed, Case, Implicit, AbsOverride, Lazy)

  /** Flags representing access rights */
  final val AccessFlags = Private | Protected | Local

  /** These flags are enabled from phase 1 */
  final val InitialFlags: FlagSet = ???

  /** These flags are not pickled */
  final val FlagsNotPickled = commonFlags(
    Erroneous, Lifted, Frozen)

  /** These flags are pickled */
  final val PickledFlags  = InitialFlags &~ FlagsNotPickled

  /** A value that's unstable unless complemented with a Stable flag */
  final val UnstableValue = oneOf(Mutable, Method, ByNameParam)

  /** Labeled private[this] */
  final val PrivateLocal = allOf(Private, Local)

  /** Labeled `private` or `protected[local]` */
  final val PrivateOrLocal = oneOf(Private, Local)

  /** Java symbol which is `protected` and `static` */
  final val StaticProtected = allOf(Java, Protected, Static)

  /** Labeled `protected[this]` */
  final val ProtectedLocal = allOf(Protected, Local)
}