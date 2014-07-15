package dotty.tools.dotc.core

import language.implicitConversions

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

    /** The intersection of this flag set and the given flag set */
    def & (that: FlagSet) = FlagSet(bits & that.bits)

    /** The intersection of this flag set with the complement of the given flag set */
    def &~ (that: FlagSet) = {
      val tbits = bits & KINDFLAGS
      if ((tbits & that.bits) == 0) this
      else FlagSet(tbits | ((this.bits & ~that.bits) & ~KINDFLAGS))
    }

    /** Does this flag set have a non-empty intersection with the given flag set?
     *  This means that both the kind flags and the carrier bits have non-empty intersection.
     */
    def is(flags: FlagSet): Boolean = {
      val fs = bits & flags.bits
      (fs & KINDFLAGS) != 0 && (fs & ~KINDFLAGS) != 0
    }

   /** Does this flag set have a non-empty intersection with the given flag set,
    *  and at the same time contain none of the flags in the `butNot` set?
    */
    def is(flags: FlagSet, butNot: FlagSet): Boolean = is(flags) && !is(butNot)

    /** Does this flag set have all of the flags in given flag conjunction?
     *  Pre: The intersection of the typeflags of both sets must be non-empty.
     */
    def is(flags: FlagConjunction): Boolean = {
      val fs = bits & flags.bits
      (fs & KINDFLAGS) != 0 &&
      (fs >>> TYPESHIFT) == (flags.bits >>> TYPESHIFT)
    }

    /** Does this flag set have all of the flags in given flag conjunction?
     *  and at the same time contain none of the flags in the `butNot` set?
     *  Pre: The intersection of the typeflags of both sets must be non-empty.
     */
    def is(flags: FlagConjunction, butNot: FlagSet): Boolean = is(flags) && !is(butNot)

    def isEmpty = (bits & ~KINDFLAGS) == 0

    /** Is this flag set a subset of that one? */
    def <= (that: FlagSet) = (bits & that.bits) == bits

    /** Does this flag set apply to terms? */
    def isTermFlags = (bits & TERMS) != 0

    /** Does this flag set apply to terms? */
    def isTypeFlags = (bits & TYPES) != 0

    /** This flag set with all flags transposed to be type flags */
    def toTypeFlags = if (bits == 0) this else FlagSet(bits & ~KINDFLAGS | TYPES)

    /** This flag set with all flags transposed to be term flags */
    def toTermFlags = if (bits == 0) this else FlagSet(bits & ~KINDFLAGS | TERMS)

    /** This flag set with all flags transposed to be common flags */
    def toCommonFlags = if (bits == 0) this else FlagSet(bits | KINDFLAGS)

    /** The number of non-kind flags in this set */
    def numFlags: Int = java.lang.Long.bitCount(bits & ~KINDFLAGS)

    /** The lowest non-kind bit set in this flagset */
    def firstBit: Int = java.lang.Long.numberOfTrailingZeros(bits & ~KINDFLAGS)

    /** The  list of non-empty names of flags with given index idx that are set in this FlagSet */
    private def flagString(idx: Int): List[String] =
      if ((bits & (1L << idx)) == 0) Nil
      else {
        def halfString(kind: Int) =
          if ((bits & (1L << kind)) != 0) flagName(idx)(kind) else ""
        val termFS = halfString(TERMindex)
        val typeFS = halfString(TYPEindex)
        val strs = termFS :: (if (termFS == typeFS) Nil else typeFS :: Nil)
        strs filter (_.nonEmpty)
      }

    /** The list of non-empty names of flags that are set in this FlagSet */
    def flagStrings: Seq[String] = {
      val rawStrings = (2 to MaxFlag).flatMap(flagString)
      if (this is Local)
        rawStrings.filter(_ != "<local>").map {
          case "private" => "private[this]"
          case "protected" => "protected[this]"
          case str => str
        }
      else rawStrings
    }

    /** The string representation of this flag set */
    override def toString = flagStrings.mkString(" ")
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

  private var flagName = Array.fill(64, 2)("")

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
  def union(flagss: FlagSet*) = (EmptyFlags /: flagss)(_ | _)

  /** The conjunction of all flags in given flag set */
  def allOf(flagss: FlagSet*) = {
    assert(flagss forall (_.numFlags == 1), "Flags.allOf doesn't support flag " + flagss.find(_.numFlags != 1))
    FlagConjunction(union(flagss: _*).bits)
  }

  def commonFlags(flagss: FlagSet*) = union(flagss.map(_.toCommonFlags): _*)

  /** The empty flag set */
  final val EmptyFlags = FlagSet(0)

  /** The undefined flag set */
  final val UndefinedFlags = FlagSet(~KINDFLAGS)

  // Available flags:

  /** Labeled with `private` modifier */
  final val Private = commonFlag(2, "private")

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
  final val MethodOrHKCommon = commonFlag(7, "<method>")
  final val Method = MethodOrHKCommon.toTermFlags
  final val HigherKinded = MethodOrHKCommon.toTypeFlags

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

  /** A value or variable accessor (getter or setter) */
  final val Accessor = termFlag(11, "<accessor>")

  /** Labeled with `sealed` modifier (sealed class) */
  final val Sealed = typeFlag(11, "sealed")

  final val AccessorOrSealed = Accessor.toCommonFlags

 /** A mutable var */
  final val Mutable = termFlag(12, "mutable")

  /** Class symbol is defined in this/superclass constructor. */
  final val InConstructor = typeFlag(12, "<inconstructor>")

  /** Symbol is local to current class (i.e. private[this] or protected[this]
   *  pre: Private or Protected are also set
   */
  final val Local = commonFlag(13, "<local>")

  /** A field generated for a primary constructor parameter (no matter if it's a 'val' or not),
   *  or an accessor of such a field.
   */
  final val ParamAccessor = commonFlag(14, "<paramaccessor>")
  final val TermParamAccessor = ParamAccessor.toTermFlags
  final val TypeParamAccessor = ParamAccessor.toTypeFlags

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

  /** Symbol's name is expanded */
  final val ExpandedName = commonFlag(19, "<expandedname>")

  /** A covariant type variable */
  final val CovariantCommon = commonFlag(20, "<covariant>")
  final val Covariant = CovariantCommon.toTypeFlags

  final val ContravariantCommon = commonFlag(21, "<contravariant>")
  final val Contravariant = ContravariantCommon.toTypeFlags

  /** Method is a label. */
  final val Label = termFlag(22, "<label>")

  /** A trait that has only abstract methods as members
   *  (and therefore can be represented by a Java interface
   */
  final val Interface = typeFlag(22, "interface")

  /** Labeled with `abstract` modifier (an abstract class)
   *  Note: You should never see Abstract on any symbol except a class.
   *  Note: the flag counts as common, because it can be combined with OVERRIDE in a term.
   */
  final val Abstract = commonFlag(23, "abstract")

  /** Labeled with of abstract & override (needed?) */
  final val AbsOverride = termFlag(24, "abstract override")

  /** Method is assumed to be stable */
  final val Stable = termFlag(25, "<stable>")

  /** Info can be refined during GADT pattern match */
  final val GADTFlexType = typeFlag(25, "<gadt-flex-type>")

  /** A case parameter (or its accessor, or a GADT skolem) */
  final val CaseAccessor = termFlag(26, "<caseaccessor>")

  /** An type parameter which is an alias for some other (non-visible) type parameter */
  final val TypeArgument = typeFlag(26, "<type-param-inst>")

  final val CaseAccessorOrTypeArgument = CaseAccessor.toCommonFlags

  /** A super accessor */
  final val SuperAccessor = termFlag(27, "<superaccessor>")

  /** An unpickled Scala 2.x class */
  final val Scala2x = typeFlag(27, "<scala-2.x>")

  /** A method that has default params */ // TODO: drop
  final val DefaultParameterized = termFlag(28, "<defaultparam>")

  /** Symbol is initialized to the default value, e.g. var x: T = _ */
  final val DefaultInit = termFlag(29, "<defaultinit>")

  /** Symbol is a macro */
  final val Macro = commonFlag(30, "<macro>")

  /** Symbol is defined by a Java class */
  final val JavaDefined = commonFlag(31, "<java>")

  /** Symbol is implemented as a Java static */
  final val Static = commonFlag(32, "<static>")

  /** Variable is accessed from nested function. */
  final val Captured = termFlag(33, "<captured>")

  /** Symbol should be ignored when typechecking; will be marked ACC_SYNTHETIC in bytecode */
  final val Artifact = commonFlag(34, "<artifact>")

  /** A bridge method. Set by Erasure */
  final val Bridge = termFlag(35, "<bridge>")

  /** Symbol is a Java varargs bridge */
  final val VBridge = termFlag(36, "<vbridge>")

  /** Symbol is a method which should be marked ACC_SYNCHRONIZED */
  final val Synchronized = termFlag(37, "<synchronized>")

  /** Symbol is a Java-style varargs method */
  final val JavaVarargs = termFlag(38, "<varargs>")

  /** Symbol is a Java default method */
  final val DefaultMethod = termFlag(39, "<defaultmethod>")

  // Flags following this one are not pickled

  /** Denotation is in train of being loaded and completed, used to catch cyclic dependencies */
  final val Touched = commonFlag(48, "<touched>")

  /** Class is not allowed to accept new members because fingerprint of subclass has been taken */
  final val Frozen = commonFlag(49, "<frozen>")

  /** An error symbol */
  final val Erroneous = commonFlag(50, "<is-error>")

  /** Class has been lifted out to package level, local value has been lifted out to class level */
  final val Lifted = commonFlag(51, "<lifted>")

  /** Term member has been mixed in */
  final val MixedIn = commonFlag(52, "<mixedin>")

  /** Symbol is a generated specialized member */
  final val Specialized = commonFlag(53, "<specialized>")

  /** Symbol is a self name */
  final val SelfName = termFlag(54, "<selfname>")

  /** Symbol is an implementation class */
  final val ImplClass = typeFlag(54, "<implclass>")

  final val SelfNameOrImplClass = SelfName.toCommonFlags

  /** An existentially bound symbol (Scala 2.x only) */
  final val Scala2ExistentialCommon = commonFlag(55, "<existential>")
  final val Scala2Existential = Scala2ExistentialCommon.toTypeFlags

  /** An overloaded symbol (Scala 2.x only) */
  final val Scala2Overloaded = termFlag(56, "<overloaded>")

  /** A module variable (Scala 2.x only) */
  final val Scala2ModuleVar = termFlag(57, "<modulevar>")

  /** A definition that's initialized before the super call (Scala 2.x only) */
  final val Scala2PreSuper = termFlag(58, "<presuper>")

  /** A method that is known to have inherited default parameters */
  final val InheritedDefaultParams = termFlag(59, "<inherited-default-param>")

  /** A method that is known to no default parameters */
  final val NoDefaultParams = termFlag(60, "<no-default-param>")

  /** A denotation that is valid in all run-ids */
  final val Permanent = commonFlag(61, "<permanent>")

// --------- Combined Flag Sets and Conjunctions ----------------------

  /** Flags representing source modifiers */
  final val SourceModifierFlags =
    commonFlags(Private, Protected, Abstract, Final,
     Sealed, Case, Implicit, Override, AbsOverride, Lazy)

  /** Flags representing modifiers that can appear in trees */
  final val ModifierFlags =
    SourceModifierFlags | Module | Param | Synthetic | Package | Local
      // | Trait is subsumed by commonFlags(Lazy) from SourceModifierFlags

  assert(ModifierFlags.isTermFlags && ModifierFlags.isTypeFlags)

  /** Flags representing access rights */
  final val AccessFlags = Private | Protected | Local

  /** Flags guaranteed to be set upon symbol creation */
  final val FromStartFlags =
    AccessFlags | Module | Package | Deferred | MethodOrHKCommon | Param | Scala2ExistentialCommon | Touched |
    Static | CovariantCommon | ContravariantCommon | ExpandedName | AccessorOrSealed |
    CaseAccessorOrTypeArgument | Frozen | Erroneous | ImplicitCommon | Permanent | SelfNameOrImplClass

  assert(FromStartFlags.isTermFlags && FromStartFlags.isTypeFlags)
  // TODO: Should check that FromStartFlags do not change in completion

  /** A value that's unstable unless complemented with a Stable flag */
  final val UnstableValue = Mutable | Method

  /** Flags that express the variance of a type parameter. */
  final val VarianceFlags = Covariant | Contravariant

  /** Flags that are passed from a type parameter of a class to a refinement symbol
    * that sets the type parameter */
  final val RetainedTypeArgFlags = VarianceFlags | ExpandedName | Protected | Local

  /** Modules always have these flags set */
  final val ModuleCreationFlags = ModuleVal | Final | Stable

  /** Module classes always have these flags set */
  final val ModuleClassCreationFlags = ModuleClass | Final

  /** The flags of the self symbol */
  final val SelfSymFlags = Private | Local | Deferred

  /** The flags of a type parameter */
  final val TypeParamCreationFlags = TypeParam | Deferred | Protected | Local

  /** Flags that can apply to both a module val and a module class, except those that
    *  are added at creation anyway
    */
  final val RetainedModuleValAndClassFlags: FlagSet =
    AccessFlags | Package | Case |
    Synthetic | ExpandedName | JavaDefined | Static | Artifact |
    Erroneous | Lifted | MixedIn | Specialized

  /** Flags that can apply to a module val */
  final val RetainedModuleValFlags: FlagSet = RetainedModuleValAndClassFlags |
    Override | Final | Method | Implicit | Lazy |
    Accessor | AbsOverride | Stable | Captured | Synchronized

  /** Flags that can apply to a module class */
  final val RetainedModuleClassFlags: FlagSet = RetainedModuleValAndClassFlags |
    InConstructor | ImplClass

  /** Packages and package classes always have these flags set */
  final val PackageCreationFlags =
    Module | Package | Final | JavaDefined | Static

  /** These flags are pickled */
  final val PickledFlags = flagRange(FirstFlag, FirstNotPickledFlag)

  /** An abstract class or a trait */
  final val AbstractOrTrait = Abstract | Trait

  /** Labeled `private` or `protected[local]` */
  final val PrivateOrLocal = Private | Local

  /** Either a module or a final class */
  final val ModuleOrFinal = ModuleClass | Final

  /** Either mutable or lazy */
  final val MutableOrLazy = Mutable | Lazy

  /** Labeled `private` or `final` */
  final val PrivateOrFinal = Private | Final

  /** A type parameter with synthesized name */
  final val ExpandedTypeParam = allOf(ExpandedName, TypeParam)

  /** A parameter or parameter accessor */
  final val ParamOrAccessor = Param | ParamAccessor

  /** A covariant type parameter instance */
  final val LocalCovariant = allOf(Local, Covariant)

  /** A contravariant type parameter instance */
  final val LocalContravariant = allOf(Local, Contravariant)

  /** Has defined or inherited default parameters */
  final val HasDefaultParams = DefaultParameterized | InheritedDefaultParams

  /** Is valid forever */
  final val ValidForever = Package | Permanent

  /** Is a default parameter in Scala 2*/
  final val DefaultParameter = allOf(Param, DefaultParameterized)

  /** A Java interface */
  final val JavaInterface = allOf(JavaDefined, Trait)

  /** A Java companion object */
  final val JavaModule = allOf(JavaDefined, Module)

  /** Labeled private[this] */
  final val PrivateLocal = allOf(Private, Local)

  /** A private parameter accessor */
  final val PrivateLocalParamAccessor = allOf(Private, Local, ParamAccessor)

  /** A private parameter */
  final val PrivateLocalParam = allOf(Private, Local, Param)

  /** A local parameter */
  final val ParamAndLocal = allOf(Param, Local)

  /** Labeled protected[this] */
  final val ProtectedLocal = allOf(Protected, Local)

  /** Java symbol which is `protected` and `static` */
  final val StaticProtected = allOf(JavaDefined, Protected, Static)

  final val AbstractFinal = allOf(Abstract, Final)
  final val AbstractSealed = allOf(Abstract, Sealed)
  final val SyntheticArtifact = allOf(Synthetic, Artifact)
  final val SyntheticTermParam = allOf(Synthetic, TermParam)
  final val SyntheticTypeParam = allOf(Synthetic, TypeParam)
  final val SyntheticCase = allOf(Synthetic, Case)
  final val AbstractAndOverride = allOf(Abstract, Override)

  implicit def conjToFlagSet(conj: FlagConjunction): FlagSet =
    FlagSet(conj.bits)
}