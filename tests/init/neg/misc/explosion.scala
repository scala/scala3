import scala.language.implicitConversions

object Flags {

  /** A FlagSet represents a set of flags. Flags are encoded as follows:
   *  The first two bits indicate whether a flagset applies to terms,
   *  to types, or to both.  Bits 2..63 are available for properties
   *  and can be doubly used for terms and types.
   */
  case class FlagSet(val bits: Long) extends AnyVal {

    /** The union of this flag set and the given flag set
     *  Combining two FlagSets with `|` will give a FlagSet
     *  that has the intersection of the applicability to terms/types
     *  of the two flag sets. It is checked that the intersection is not empty.
     */
    def | (that: FlagSet): FlagSet =
      if (bits == 0) that
      else if (that.bits == 0) this
      else {
        val tbits = bits & that.bits & KINDFLAGS
        if (tbits == 0)
          assert(false, s"illegal flagset combination: $this and $that")
        FlagSet(tbits | ((this.bits | that.bits) & ~KINDFLAGS))
      }

    /** The intersection of this flag set and the given flag set */
    def & (that: FlagSet) = FlagSet(bits & that.bits)

    /** This flag set with all flags transposed to be type flags */
    def toTypeFlags = if (bits == 0) this else FlagSet(bits & ~KINDFLAGS | TYPES)

    /** This flag set with all flags transposed to be term flags */
    def toTermFlags = if (bits == 0) this else FlagSet(bits & ~KINDFLAGS | TERMS)

    /** This flag set with all flags transposed to be common flags */
    def toCommonFlags = if (bits == 0) this else FlagSet(bits | KINDFLAGS)

    /** The number of non-kind flags in this set */
    def numFlags: Int = java.lang.Long.bitCount(bits & ~KINDFLAGS)
  }

  /** A class representing flag sets that should be tested
   *  conjunctively. I.e. for a flag conjunction `fc`,
   *  `x is fc` tests whether `x` contains all flags in `fc`.
   */
  case class FlagConjunction(bits: Long) {
    override def toString = FlagSet(bits).toString
  }

  private final val TYPESHIFT = 2
  private final val TERMindex = 0
  private final val TYPEindex = 1
  private final val TERMS = 1 << TERMindex
  private final val TYPES = 1 << TYPEindex
  private final val KINDFLAGS = TERMS | TYPES

  private final val FirstFlag = 2
  private final val FirstNotPickledFlag = 48
  private final val MaxFlag = 63

  private val flagName = Array.fill(64, 2)("")

  private def isDefinedAsFlag(idx: Int) = flagName(idx) exists (_.nonEmpty)

  /** The flag set containing all defined flags of either kind whose bits
   *  lie in the given range
   */
  private def flagRange(start: Int, end: Int) =
    FlagSet((KINDFLAGS.toLong /: (start until end)) ((bits, idx) =>
      if (isDefinedAsFlag(idx)) bits | (1L << idx) else bits))

  /** The flag with given index between 2 and 63 which applies to terms.
   *  Installs given name as the name of the flag. */
  private def termFlag(index: Int, name: String): FlagSet = {
    flagName(index)(TERMindex) = name
    FlagSet(TERMS | (1L << index))
  }

   /** The flag with given index between 2 and 63 which applies to types.
   *  Installs given name as the name of the flag. */
  private def typeFlag(index: Int, name: String): FlagSet = {
    flagName(index)(TYPEindex) = name
    FlagSet(TYPES | (1L << index))
  }

  /** The flag with given index between 2 and 63 which applies to both terms and types
   *  Installs given name as the name of the flag. */
  private def commonFlag(index: Int, name: String): FlagSet = {
    flagName(index)(TERMindex) = name
    flagName(index)(TYPEindex) = name
    FlagSet(TERMS | TYPES | (1L << index))
  }

  /** The union of all flags in given flag set */
  def union(flagss: FlagSet*): FlagSet = {
    var flag = EmptyFlags
    for (f <- flagss)
      flag |= f
    flag
  }

  def commonFlags(flagss: FlagSet*) = union(flagss.map(_.toCommonFlags): _*)

  /** The empty flag set */
  final val EmptyFlags = FlagSet(0)

  /** The undefined flag set */
  final val UndefinedFlags = FlagSet(~KINDFLAGS)

  // Available flags:

  /** Labeled with `private` modifier */
  final val Private = commonFlag(2, "private")
  final val PrivateTerm = Private.toTermFlags
  final val PrivateType = Private.toTypeFlags

  /** Labeled with `protected` modifier */
  final val Protected = commonFlag(3, "protected")

  /** Labeled with `override` modifier */
  final val Override = commonFlag(4, "override")

  /** A declared, but not defined member */
  final val Deferred = commonFlag(5, "<deferred>")
  final val DeferredTerm = Deferred.toTermFlags
  final val DeferredType = Deferred.toTypeFlags

  /** Labeled with `final` modifier */
  final val Final = commonFlag(6, "final")

  /** A method symbol. */
  final val Method = termFlag(7, "<method>")
  final val HigherKinded = typeFlag(7, "<higher kinded>")

  /** A (term or type) parameter to a class or method */
  final val Param     = commonFlag(8, "<param>")
  final val TermParam = Param.toTermFlags
  final val TypeParam = Param.toTypeFlags

  /** Labeled with `implicit` modifier (implicit value) */
  final val ImplicitCommon = commonFlag(9, "implicit")
  final val Implicit = ImplicitCommon.toTermFlags

  /** Labeled with `lazy` (a lazy val). */
  final val Lazy = termFlag(10, "lazy")

  /** A trait */
  final val Trait = typeFlag(10, "<trait>")

  final val LazyOrTrait = Lazy.toCommonFlags

  /** A value or variable accessor (getter or setter) */
  final val Accessor = termFlag(11, "<accessor>")

  /** Labeled with `sealed` modifier (sealed class) */
  final val Sealed = typeFlag(11, "sealed")

  final val AccessorOrSealed = Accessor.toCommonFlags

 /** A mutable var */
  final val Mutable = termFlag(12, "mutable")

  /** Symbol is local to current class (i.e. private[this] or protected[this]
   *  pre: Private or Protected are also set
   */
  final val Local = commonFlag(13, "<local>")

  /** A field generated for a primary constructor parameter (no matter if it's a 'val' or not),
   *  or an accessor of such a field.
   */
  final val ParamAccessor = termFlag(14, "<paramaccessor>")

    /** A value or class implementing a module */
  final val Module = commonFlag(15, "module")
  final val ModuleVal = Module.toTermFlags
  final val ModuleClass = Module.toTypeFlags

   /** A value or class representing a package */
  final val Package = commonFlag(16, "<package>")
  final val PackageVal = Package.toTermFlags
  final val PackageClass = Package.toTypeFlags

  /** A case class or its companion object */
  final val Case = commonFlag(17, "case")
  final val CaseClass = Case.toTypeFlags
  final val CaseVal = Case.toTermFlags

  /** A compiler-generated symbol, which is visible for type-checking
   *  (compare with artifact)
   */
  final val Synthetic = commonFlag(18, "<synthetic>")

  /** Labelled with `inline` modifier */
  final val Inline = commonFlag(19, "inline")

  /** A covariant type variable / an outer accessor */
  final val CovariantOrOuter = commonFlag(20, "")
  final val Covariant = typeFlag(20, "<covariant>")
  final val OuterAccessor = termFlag(20, "<outer accessor>")

  /** A contravariant type variable / a label method */
  final val ContravariantOrLabel = commonFlag(21, "")
  final val Contravariant = typeFlag(21, "<contravariant>")
  final val Label = termFlag(21, "<label>")


  /** A trait that has only abstract methods as members
   *  and therefore can be represented by a Java interface.
   *  Warning: flag is set during regular typer pass, should be tested only after typer.
   */
  final val PureInterface = typeFlag(22, "interface")

  /** Labeled with of abstract & override */
  final val AbsOverride = termFlag(22, "abstract override")

  /** Labeled with `abstract` modifier (an abstract class)
   *  Note: You should never see Abstract on any symbol except a class.
   *  Note: the flag counts as common, because it can be combined with OVERRIDE in a term.
   */
  final val Abstract = commonFlag(23, "abstract")

  /** Lazy val or method is known or assumed to be stable and realizable */
  final val Stable = termFlag(24, "<stable>")

  /** A case parameter accessor */
  final val CaseAccessor = termFlag(25, "<caseaccessor>")

  /** A super accessor */
  final val Scala2SuperAccessor = termFlag(26, "<superaccessor>")

  /** An unpickled Scala 2.x class */
  final val Scala2x = typeFlag(26, "<scala-2.x>")

  final val SuperAccessorOrScala2x = Scala2x.toCommonFlags

  /** A method that has default params */
  final val DefaultParameterized = termFlag(27, "<defaultparam>")

  /** Symbol is defined by a Java class */
  final val JavaDefined = commonFlag(30, "<java>")

  /** Symbol is implemented as a Java static */
  final val JavaStatic = commonFlag(31, "<static>")
  final val JavaStaticTerm = JavaStatic.toTermFlags
  final val JavaStaticType = JavaStatic.toTypeFlags

  /** Trait does not have fields or initialization code.
   *  Warning: flag is set during regular typer pass, should be tested only after typer.
   */
  final val NoInits = typeFlag(32, "<noInits>")

  /** Variable is accessed from nested function. */
  final val Captured = termFlag(32, "<captured>")

  /** Symbol should be ignored when typechecking; will be marked ACC_SYNTHETIC in bytecode */
  final val Artifact = commonFlag(33, "<artifact>")

  /** A bridge method. Set by Erasure */
  final val Bridge = termFlag(34, "<bridge>")

  /** Symbol is a method which should be marked ACC_SYNCHRONIZED */
  final val Synchronized = termFlag(36, "<synchronized>")

  /** Symbol is a Java-style varargs method */
  final val JavaVarargs = termFlag(37, "<varargs>")

  /** Symbol is a Java default method */
  final val DefaultMethod = termFlag(38, "<defaultmethod>")

  /** Symbol is an enum class or enum case (if used with case) */
  final val Enum = commonFlag(40, "<enum>")

  /** Labeled with `erased` modifier (erased value)  */
  final val Erased = termFlag(42, "erased")

  // Flags following this one are not pickled

  /** Symbol is not a member of its owner */
  final val NonMember = commonFlag(45, "<non-member>")

  /** Denotation is in train of being loaded and completed, used to catch cyclic dependencies */
  final val Touched = commonFlag(48, "<touched>")

  /** Class has been lifted out to package level, local value has been lifted out to class level */
  final val Lifted = commonFlag(51, "<lifted>")

  /** Term member has been mixed in */
  final val MixedIn = commonFlag(52, "<mixedin>")

  /** Symbol is a generated specialized member */
  final val Specialized = commonFlag(53, "<specialized>")

  /** Symbol is a self name */
  final val SelfName = termFlag(54, "<selfname>")

  /** Symbol is an implementation class of a Scala2 trait */
  final val ImplClass = typeFlag(54, "<implclass>")

  final val SelfNameOrImplClass = SelfName.toCommonFlags

  /** An existentially bound symbol (Scala 2.x only) */
  final val Scala2ExistentialCommon = commonFlag(55, "<existential>")
  final val Scala2Existential = Scala2ExistentialCommon.toTypeFlags

  /** A module variable (Scala 2.x only) */
  final val Scala2ModuleVar = termFlag(57, "<modulevar>")

  /** A Scala 2.12 trait that has been augmented with static members */
  final val Scala_2_12_Augmented = typeFlag(57, "<scala_2_12_augmented>")

  /** A definition that's initialized before the super call (Scala 2.x only) */
  final val Scala2PreSuper = termFlag(58, "<presuper>")

  /** A Scala 2.12 or higher trait */
  final val Scala_2_12_Trait = typeFlag(58, "<scala_2_12_trait>")

  /** A macro */
  final val Macro = commonFlag(59, "<macro>")

  /** A method that is known to have inherited default parameters */
  final val InheritedDefaultParams = termFlag(60, "<inherited-default-param>")

  /** Translation of Scala2's EXPANDEDNAME flag. This flag is never stored in
   *  symbols, is only used locally when reading the flags of a Scala2 symbol.
   *  It's therefore safe to share the code with `InheritedDefaultParams` because
   *  the latter is never present in Scala2 unpickle info.
   */
  final val Scala2ExpandedName = InheritedDefaultParams.toCommonFlags

  /** A method that is known to have no default parameters */
  final val NoDefaultParams = termFlag(61, "<no-default-param>")

  /** A type symbol with provisional empty bounds */
  final val Provisional = typeFlag(61, "<provisional>")

  /** A denotation that is valid in all run-ids */
  final val Permanent = commonFlag(62, "<permanent>")


  /** The conjunction of all flags in given flag set */
  def allOf(flags1: FlagSet, flags2: FlagSet): FlagConjunction = {
    assert(flags1.numFlags == 1 && flags2.numFlags == 1, "Flags.allOf doesn't support flag " + (if (flags1.numFlags != 1) flags1 else flags2))
    FlagConjunction((flags1 | flags2).bits)
  }

  /** The conjunction of all flags in given flag set */
  def allOf(flags1: FlagSet, flags2: FlagSet, flags3: FlagSet, flagss: FlagSet*): FlagConjunction = {
    val flags0 = allOf(flags1, flags2) | flags3
    assert(flags3.numFlags == 1 && flagss.forall(_.numFlags == 1), "Flags.allOf doesn't support flag " + (if (flags3.numFlags != 1) flags3 else flagss.find(_.numFlags != 1)))
    FlagConjunction((flags0 | union(flagss: _*)).bits)
  }

  implicit def conjToFlagSet(conj: FlagConjunction): FlagSet =
    FlagSet(conj.bits)

  final val AbstractFinal = allOf(Abstract, Final)
  final val AbstractSealed = allOf(Abstract, Sealed)
  final val SyntheticArtifact = allOf(Synthetic, Artifact)
  final val SyntheticModule = allOf(Synthetic, Module)
  final val SyntheticTermParam = allOf(Synthetic, TermParam)
  final val SyntheticTypeParam = allOf(Synthetic, TypeParam)
  final val SyntheticCase = allOf(Synthetic, Case)
  final val AbstractAndOverride = allOf(Abstract, Override)
  final val Scala2Trait = allOf(Scala2x, Trait)

  /** Flags representing access rights */
  final val AccessFlags = Private | Protected | Local

  final val RetainedModuleValAndClassFlags: FlagSet =
    AccessFlags | Package | Case |
    Synthetic | JavaDefined | JavaStatic | Artifact |
    Lifted | MixedIn | Specialized

  /** Flags that can apply to a module val */
  final val RetainedModuleValFlags: FlagSet = RetainedModuleValAndClassFlags |
    Override | Final | Method | Implicit | Lazy |
    Accessor | AbsOverride | Stable | Captured | Synchronized | Erased
}
