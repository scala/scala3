package dotty.tools.dotc
package core

import language.implicitConversions

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
    def & (that: FlagSet): FlagSet = FlagSet(bits & that.bits)

    /** The intersection of this flag set with the complement of the given flag set */
    def &~ (that: FlagSet): FlagSet = {
      val tbits = bits & KINDFLAGS
      if ((tbits & that.bits) == 0) this
      else FlagSet(tbits | ((this.bits & ~that.bits) & ~KINDFLAGS))
    }

    def ^ (that: FlagSet) =
      FlagSet((bits | that.bits) & KINDFLAGS | (bits ^ that.bits) & ~KINDFLAGS)

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
      ((fs & KINDFLAGS) != 0 || flags.bits == 0) &&
      (fs >>> TYPESHIFT) == (flags.bits >>> TYPESHIFT)
    }

    /** Does this flag set have all of the flags in given flag conjunction?
     *  and at the same time contain none of the flags in the `butNot` set?
     *  Pre: The intersection of the typeflags of both sets must be non-empty.
     */
    def is(flags: FlagConjunction, butNot: FlagSet): Boolean = is(flags) && !is(butNot)

    def isEmpty: Boolean = (bits & ~KINDFLAGS) == 0

    /** Is this flag set a subset of that one? */
    def <= (that: FlagSet): Boolean = (bits & that.bits) == bits

    /** Does this flag set apply to terms? */
    def isTermFlags: Boolean = (bits & TERMS) != 0

    /** Does this flag set apply to terms? */
    def isTypeFlags: Boolean = (bits & TYPES) != 0

    /** This flag set with all flags transposed to be type flags */
    def toTypeFlags: FlagSet = if (bits == 0) this else FlagSet(bits & ~KINDFLAGS | TYPES)

    /** This flag set with all flags transposed to be term flags */
    def toTermFlags: FlagSet = if (bits == 0) this else FlagSet(bits & ~KINDFLAGS | TERMS)

    /** This flag set with all flags transposed to be common flags */
    def toCommonFlags: FlagSet = if (bits == 0) this else FlagSet(bits | KINDFLAGS)

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
    def flagStrings(privateWithin: String): Seq[String] = {
      val rawStrings = (2 to MaxFlag).flatMap(flagString)
      val scopeStr =
        if (this is Local) "this"
        else privateWithin
      if (privateWithin != "")
        rawStrings.filter(_ != "<local>").map {
          case "private" => s"private[$scopeStr]"
          case "protected" => s"protected[$scopeStr]"
          case str => str
        }
      else rawStrings
    }

    /** The string representation of this flag set */
    override def toString: String = flagStrings("").mkString(" ")
  }

  def termFlagSet(x: Long) = FlagSet(TERMS | x)

  /** A class representing flag sets that should be tested
   *  conjunctively. I.e. for a flag conjunction `fc`,
   *  `x is fc` tests whether `x` contains all flags in `fc`.
   */
  case class FlagConjunction(bits: Long) {
    override def toString: String = FlagSet(bits).toString
    def | (fs: FlagSet): FlagConjunction = FlagConjunction((FlagSet(bits) | fs).bits)
  }

  def termFlagConjunction(x: Long) = FlagConjunction(TERMS | x)

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

  def allOf(flags: FlagSet) = FlagConjunction(flags.bits)

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

  def commonFlags(flagss: FlagSet*): FlagSet = union(flagss.map(_.toCommonFlags): _*)

  /** The empty flag set */
  final val EmptyFlags: FlagSet = FlagSet(0)

  final val EmptyFlagConjunction = FlagConjunction(0)

  /** The undefined flag set */
  final val UndefinedFlags: FlagSet = FlagSet(~KINDFLAGS)

  // Available flags:

  /** Labeled with `private` modifier */
  final val Private: FlagSet = commonFlag(2, "private")
  final val PrivateTerm: FlagSet = Private.toTermFlags
  final val PrivateType: FlagSet = Private.toTypeFlags

  /** Labeled with `protected` modifier */
  final val Protected: FlagSet = commonFlag(3, "protected")

  /** Labeled with `override` modifier */
  final val Override: FlagSet = commonFlag(4, "override")

  /** A declared, but not defined member */
  final val Deferred: FlagSet = commonFlag(5, "<deferred>")
  final val DeferredTerm: FlagSet = Deferred.toTermFlags
  final val DeferredType: FlagSet = Deferred.toTypeFlags

  /** Labeled with `final` modifier */
  final val Final: FlagSet = commonFlag(6, "final")

  /** A method symbol. */
  final val Method: FlagSet = termFlag(7, "<method>")
  final val HigherKinded: FlagSet = typeFlag(7, "<higher kinded>")

  /** A (term or type) parameter to a class or method */
  final val Param: FlagSet     = commonFlag(8, "<param>")
  final val TermParam: FlagSet = Param.toTermFlags
  final val TypeParam: FlagSet = Param.toTypeFlags

  /** Labeled with `implicit` modifier (implicit value) */
  final val Implicit: FlagSet = commonFlag(9, "implicit")
  final val ImplicitTerm: FlagSet = Implicit.toTermFlags

  /** Labeled with `lazy` (a lazy val). */
  final val Lazy: FlagSet = termFlag(10, "lazy")

  /** A trait */
  final val Trait: FlagSet = typeFlag(10, "<trait>")

  final val LazyOrTrait: FlagSet = Lazy.toCommonFlags

  /** A value or variable accessor (getter or setter) */
  final val Accessor: FlagSet = termFlag(11, "<accessor>")

  /** Labeled with `sealed` modifier (sealed class) */
  final val Sealed: FlagSet = typeFlag(11, "sealed")

  final val AccessorOrSealed: FlagSet = Accessor.toCommonFlags

  /** A mutable var */
  final val Mutable: FlagSet = termFlag(12, "mutable")

  /** An opaque type or a class containing one */
  final val Opaque: FlagSet = typeFlag(12, "opaque")

  final val MutableOrOpaque: FlagSet = Mutable.toCommonFlags

  /** Symbol is local to current class (i.e. private[this] or protected[this]
   *  pre: Private or Protected are also set
   */
  final val Local: FlagSet = commonFlag(13, "<local>")

  /** A field generated for a primary constructor parameter (no matter if it's a 'val' or not),
   *  or an accessor of such a field.
   */
  final val ParamAccessor: FlagSet = termFlag(14, "<paramaccessor>")

  /** A value or class implementing a module */
  final val Module: FlagSet = commonFlag(15, "module")
  final val ModuleVal: FlagSet = Module.toTermFlags
  final val ModuleClass: FlagSet = Module.toTypeFlags

   /** A value or class representing a package */
  final val Package: FlagSet = commonFlag(16, "<package>")
  final val PackageVal: FlagSet = Package.toTermFlags
  final val PackageClass: FlagSet = Package.toTypeFlags

  /** A case class or its companion object
   *  Note: Case is also used to indicate that a symbol is bound by a pattern.
   */
  final val Case: FlagSet = commonFlag(17, "case")
  final val CaseClass: FlagSet = Case.toTypeFlags
  final val CaseVal: FlagSet = Case.toTermFlags

  /** A compiler-generated symbol, which is visible for type-checking
   *  (compare with artifact)
   */
  final val Synthetic: FlagSet = commonFlag(18, "<synthetic>")

  /** Labelled with `inline` modifier */
  final val Inline: FlagSet = commonFlag(19, "inline")

  /** A covariant type variable / an outer accessor */
  final val CovariantOrOuter: FlagSet = commonFlag(20, "")
  final val Covariant: FlagSet = typeFlag(20, "<covariant>")
  final val OuterAccessor: FlagSet = termFlag(20, "<outer accessor>")

  /** A contravariant type variable / the label of a labeled block */
  final val ContravariantOrLabel: FlagSet = commonFlag(21, "")
  final val Contravariant: FlagSet = typeFlag(21, "<contravariant>")
  final val Label: FlagSet = termFlag(21, "<label>")

  /** A trait that has only abstract methods as members
   *  and therefore can be represented by a Java interface.
   *  Warning: flag is set during regular typer pass, should be tested only after typer.
   */
  final val PureInterface: FlagSet = typeFlag(22, "interface")

  /** Labeled with of abstract & override */
  final val AbsOverride: FlagSet = termFlag(22, "abstract override")

  /** Labeled with `abstract` modifier (an abstract class)
   *  Note: You should never see Abstract on any symbol except a class.
   *  Note: the flag counts as common, because it can be combined with OVERRIDE in a term.
   */
  final val Abstract: FlagSet = commonFlag(23, "abstract")

  /** Lazy val or method is known or assumed to be stable and realizable */
  final val StableRealizable: FlagSet = termFlag(24, "<stable>")

  /** A case parameter accessor */
  final val CaseAccessor: FlagSet = termFlag(25, "<caseaccessor>")

  /** A super accessor */
  final val Scala2SuperAccessor: FlagSet = termFlag(26, "<superaccessor>")

  /** An unpickled Scala 2.x class */
  final val Scala2x: FlagSet = typeFlag(26, "<scala-2.x>")

  final val Scala2xTrait: FlagSet = Scala2x | Trait

  final val SuperAccessorOrScala2x: FlagSet = Scala2x.toCommonFlags

  /** A method that has default params */
  final val DefaultParameterized: FlagSet = termFlag(27, "<defaultparam>")

  /** An extension method */
  final val Extension = termFlag(28, "<extension>")

  /** An inferable (`given`) parameter */
  final val Given = commonFlag(29, "given")

  /** Symbol is defined by a Java class */
  final val JavaDefined: FlagSet = commonFlag(30, "<java>")

  /** Symbol is implemented as a Java static */
  final val JavaStatic: FlagSet = commonFlag(31, "<static>")
  final val JavaStaticTerm: FlagSet = JavaStatic.toTermFlags
  final val JavaStaticType: FlagSet = JavaStatic.toTypeFlags

  /** Trait does not have fields or initialization code.
   *  Warning: flag is set during regular typer pass, should be tested only after typer.
   */
  final val NoInits: FlagSet = typeFlag(32, "<noInits>")

  /** Variable is accessed from nested function. */
  final val Captured: FlagSet = termFlag(32, "<captured>")

  /** Symbol should be ignored when typechecking; will be marked ACC_SYNTHETIC in bytecode */
  final val Artifact: FlagSet = commonFlag(33, "<artifact>")

  /** A bridge method. Set by Erasure */
  final val Bridge: FlagSet = termFlag(34, "<bridge>")

  /** A proxy for an argument to an inline method */
  final val InlineProxy: FlagSet = termFlag(35, "<inline proxy>")

  /** Symbol is a method which should be marked ACC_SYNCHRONIZED */
  final val Synchronized: FlagSet = termFlag(36, "<synchronized>")

  /** Symbol is a Java-style varargs method */
  final val JavaVarargs: FlagSet = termFlag(37, "<varargs>")

  /** Symbol is a Java default method */
  final val DefaultMethod: FlagSet = termFlag(38, "<defaultmethod>")

  final val Implied: FlagSet = commonFlag(39, "implied")

  /** Symbol is an enum class or enum case (if used with case) */
  final val Enum: FlagSet = commonFlag(40, "<enum>")

  /** An export forwarder */
  final val Exported: FlagSet = commonFlag(41, "exported")

  /** Labeled with `erased` modifier (erased value)  */
  final val Erased: FlagSet = termFlag(42, "erased")

  // Flags following this one are not pickled

  /** Symbol is not a member of its owner */
  final val NonMember: FlagSet = commonFlag(45, "<non-member>")

  /** Denotation is in train of being loaded and completed, used to catch cyclic dependencies */
  final val Touched: FlagSet = commonFlag(48, "<touched>")

  /** Class has been lifted out to package level, local value has been lifted out to class level */
  final val Lifted: FlagSet = commonFlag(51, "<lifted>")

  /** Term member has been mixed in */
  final val MixedIn: FlagSet = commonFlag(52, "<mixedin>")

  /** Symbol is a generated specialized member */
  final val Specialized: FlagSet = commonFlag(53, "<specialized>")

  /** Symbol is a self name */
  final val SelfName: FlagSet = termFlag(54, "<selfname>")

  /** An existentially bound symbol (Scala 2.x only) */
  final val Scala2ExistentialCommon: FlagSet = commonFlag(55, "<existential>")
  final val Scala2Existential: FlagSet = Scala2ExistentialCommon.toTypeFlags

  /** Children were queried on this class */
  final val ChildrenQueried = typeFlag(56, "<children-queried>")

  /** A module variable (Scala 2.x only) */
  final val Scala2ModuleVar: FlagSet = termFlag(57, "<modulevar>")

  /** A Scala 2.x trait that has been partially augmented.
   *  This is set in `AugmentScala2Trait` and reset in `LinkScala2Impls`
   *  when the trait is fully augmented.
   */
  final val Scala2xPartiallyAugmented: FlagSet = typeFlag(57, "<scala-2.x-partially-augmented>")

  /** A macro */
  final val Macro: FlagSet = commonFlag(59, "<macro>")

  /** A method that is known to have inherited default parameters */
  final val InheritedDefaultParams: FlagSet = termFlag(60, "<inherited-default-param>")

  /** Translation of Scala2's EXPANDEDNAME flag. This flag is never stored in
   *  symbols, is only used locally when reading the flags of a Scala2 symbol.
   *  It's therefore safe to share the code with `InheritedDefaultParams` because
   *  the latter is never present in Scala2 unpickle info.
   */
  final val Scala2ExpandedName: FlagSet = InheritedDefaultParams.toCommonFlags

  /** A method that is known to have no default parameters */
  final val NoDefaultParams: FlagSet = termFlag(61, "<no-default-param>")

  /** A type symbol with provisional empty bounds */
  final val Provisional: FlagSet = typeFlag(61, "<provisional>")

  /** A denotation that is valid in all run-ids */
  final val Permanent: FlagSet = commonFlag(62, "<permanent>")

// --------- Combined Flag Sets and Conjunctions ----------------------

  /** Flags representing source modifiers */
  private val CommonSourceModifierFlags: FlagSet =
    commonFlags(Private, Protected, Final, Case, Implicit, Implied, Given, Override, JavaStatic)

  final val TypeSourceModifierFlags: FlagSet =
    CommonSourceModifierFlags.toTypeFlags | Abstract | Sealed | Opaque

  final val TermSourceModifierFlags: FlagSet =
    CommonSourceModifierFlags.toTermFlags | Inline | AbsOverride | Lazy | Erased

  /** Flags representing modifiers that can appear in trees */
  final val ModifierFlags: FlagSet =
    TypeSourceModifierFlags.toCommonFlags |
    TermSourceModifierFlags.toCommonFlags |
    commonFlags(Module, Param, Synthetic, Package, Local, Mutable, Trait)

  assert(ModifierFlags.isTermFlags && ModifierFlags.isTypeFlags)

  /** Flags representing access rights */
  final val AccessFlags: FlagSet = Private | Protected | Local

  /** Flags that are not (re)set when completing the denotation */
  final val FromStartFlags: FlagSet =
    Module | Package | Deferred | Method.toCommonFlags | Case |
    HigherKinded.toCommonFlags | Param | ParamAccessor.toCommonFlags |
    Scala2ExistentialCommon | MutableOrOpaque | Touched | JavaStatic |
    CovariantOrOuter | ContravariantOrLabel | CaseAccessor.toCommonFlags |
    Extension.toCommonFlags | NonMember | Implicit | Given | Implied | Permanent | Synthetic |
    SuperAccessorOrScala2x | Inline

  /** Flags that are not (re)set when completing the denotation, or, if symbol is
   *  a top-level class or object, when completing the denotation once the class
   *  file defining the symbol is loaded (which is generally before the denotation
   *  is completed)
   */
  final val AfterLoadFlags: FlagSet =
    FromStartFlags | AccessFlags | Final | AccessorOrSealed | LazyOrTrait | SelfName.toCommonFlags

  assert(FromStartFlags.isTermFlags && FromStartFlags.isTypeFlags)
  // TODO: Should check that FromStartFlags do not change in completion
  assert(AfterLoadFlags.isTermFlags && AfterLoadFlags.isTypeFlags)

  /** A value that's unstable unless complemented with a Stable flag */
  final val UnstableValue: FlagSet = Mutable | Method

  /** Flags that express the variance of a type parameter. */
  final val VarianceFlags: FlagSet = Covariant | Contravariant

  /** Flags that are passed from a type parameter of a class to a refinement symbol
    * that sets the type parameter */
  final val RetainedTypeArgFlags: FlagSet = VarianceFlags | Protected | Local

  /** Modules always have these flags set */
  final val ModuleValCreationFlags: FlagSet = ModuleVal | Lazy | Final | StableRealizable

  /** Module classes always have these flags set */
  final val ModuleClassCreationFlags: FlagSet = ModuleClass | Final

  /** Accessors always have these flags set */
  final val AccessorCreationFlags: FlagSet = Method | Accessor

  /** Pure interfaces always have these flags */
  final val PureInterfaceCreationFlags: FlagSet = Trait | NoInits | PureInterface

  final val NoInitsInterface: FlagSet = NoInits | PureInterface

  /** The flags of the self symbol */
  final val SelfSymFlags: FlagSet = Private | Local | Deferred

  /** The flags of a class type parameter */
  final val ClassTypeParamCreationFlags: FlagSet =
    TypeParam | Deferred | Private | Local

  /** Flags that can apply to both a module val and a module class, except those that
    *  are added at creation anyway
    */
  final val RetainedModuleValAndClassFlags: FlagSet =
    AccessFlags | Package | Case |
    Synthetic | JavaDefined | JavaStatic | Artifact |
    Lifted | MixedIn | Specialized

  /** Flags that can apply to a module val */
  final val RetainedModuleValFlags: FlagSet = RetainedModuleValAndClassFlags |
    Override | Final | Method | Implicit | Implied | Lazy |
    Accessor | AbsOverride | StableRealizable | Captured | Synchronized | Erased

  /** Flags that can apply to a module class */
  final val RetainedModuleClassFlags: FlagSet = RetainedModuleValAndClassFlags | Enum

  /** Packages and package classes always have these flags set */
  final val PackageCreationFlags: FlagSet =
    Module | Package | Final | JavaDefined

  /** These flags are pickled */
  final val PickledFlags: FlagSet = flagRange(FirstFlag, FirstNotPickledFlag)

  final val AnyFlags: FlagSet = flagRange(FirstFlag, MaxFlag)

  /** An abstract class or a trait */
  final val AbstractOrTrait: FlagSet = Abstract | Trait

  /** Labeled `private` or `protected[local]` */
  final val PrivateOrLocal: FlagSet = Private | Local

  /** Either a module or a final class */
  final val ModuleOrFinal: FlagSet = ModuleClass | Final

  /** Either mutable or lazy */
  final val MutableOrLazy: FlagSet = Mutable | Lazy

  /** Either method or lazy */
  final val MethodOrLazy: FlagSet = Method | Lazy

  /** Either method or module */
  final val MethodOrModule: FlagSet = Method | Module

  /** Either method or lazy or deferred */
  final val MethodOrLazyOrDeferred: FlagSet = Method | Lazy | Deferred

  /** An inline method or inline argument proxy */
  final val InlineOrProxy: FlagSet = Inline | InlineProxy

  final val ImplicitOrImplied = Implicit | Implied
  final val ImplicitOrImpliedOrGiven = Implicit | Implied | Given
  final val ImplicitOrGiven = Implicit | Given

  final val ImpliedOrGiven = Implied | Given

  final val ImplicitOrImpliedOrGivenTerm = ImplicitOrImpliedOrGiven.toTermFlags

  /** Flags retained in export forwarders */
  final val RetainedExportFlags = ImplicitOrImpliedOrGiven | Extension

  /** Assumed to be pure */
  final val StableOrErased: FlagSet = StableRealizable | Erased

  /** Labeled `private`, or `final` */
  final val EffectivelyFinal: FlagSet = Private | Final

  /** A private method */
  final val PrivateMethod: FlagConjunction = allOf(Private, Method)

  /** A private accessor */
  final val PrivateAccessor: FlagConjunction = allOf(Private, Accessor)

  /** An inline method */
  final val InlineMethod: FlagConjunction = allOf(Inline, Method)

  /** An inline by-name parameter proxy */
  final val InlineByNameProxy: FlagConjunction = allOf(InlineProxy, Method)

  /** An inline parameter */
  final val InlineParam: FlagConjunction = allOf(Inline, Param)

  /** An extension method */
  final val ExtensionMethod = allOf(Extension, Method)

  /** An implied method */
  final val SyntheticImpliedMethod: FlagConjunction = allOf(Synthetic, Implied, Method)

  /** An enum case */
  final val EnumCase: FlagConjunction = allOf(Enum, Case)
  final val EnumCaseVal: FlagConjunction = allOf(Enum, CaseVal)

  /** A term parameter or parameter accessor */
  final val TermParamOrAccessor: FlagSet = Param | ParamAccessor

  /** A lazy or deferred value */
  final val LazyOrDeferred: FlagSet = Lazy | Deferred

  /** An accessor or a synthetic symbol */
  final val AccessorOrSynthetic: FlagSet = Accessor | Synthetic

  /** A synthetic or private definition */
  final val SyntheticOrPrivate: FlagSet = Synthetic | Private

  /** A deferred term member or a parameter or parameter accessor (these don't have right hand sides) */
  final val DeferredOrTermParamOrAccessor: FlagSet = Deferred | TermParam | ParamAccessor

  /** A deferred type member or parameter (these don't have right hand sides) */
  final val DeferredOrTypeParam: FlagSet = Deferred | TypeParam

  /** value that's final or inline */
  final val FinalOrInline: FlagSet = Final | Inline

  /** class that's final or sealed */
  final val FinalOrSealed: FlagSet = Final | Sealed

  /** A covariant type parameter instance */
  final val LocalCovariant: FlagConjunction = allOf(Local, Covariant)

  /** A contravariant type parameter instance */
  final val LocalContravariant: FlagConjunction = allOf(Local, Contravariant)

  /** Has defined or inherited default parameters */
  final val HasDefaultParams: FlagSet = DefaultParameterized | InheritedDefaultParams

  /** Is valid forever */
  final val ValidForever: FlagSet = Package | Permanent | Scala2ExistentialCommon

  /** A type parameter of a class or trait */
  final val ClassTypeParam: FlagConjunction = allOf(TypeParam, Private)

  /** Is a default parameter in Scala 2*/
  final val DefaultParameter: FlagConjunction = allOf(Param, DefaultParameterized)

  /** A trait that does not need to be initialized */
  final val NoInitsTrait: FlagConjunction = allOf(Trait, NoInits)

  /** A Java interface, potentially with default methods */
  final val JavaTrait: FlagConjunction = allOf(JavaDefined, Trait, NoInits)

    /** A Java interface */ // TODO when unpickling, reconstitute from context
  final val JavaInterface: FlagConjunction = allOf(JavaDefined, Trait)

  /** A Java companion object */
  final val JavaModule: FlagConjunction = allOf(JavaDefined, Module)

  /** A Java companion object */
  final val JavaProtected: FlagConjunction = allOf(JavaDefined, Protected)

  /** A Java enum */
  final val JavaEnum: FlagConjunction = allOf(JavaDefined, Enum)

  /** A Java enum trait */
  final val JavaEnumTrait: FlagConjunction = allOf(JavaDefined, Enum)

  /** A Java enum value */
  final val JavaEnumValue: FlagConjunction = allOf(StableRealizable, JavaStatic, JavaDefined, Enum)

  /** Labeled private[this] */
  final val PrivateLocal: FlagConjunction = allOf(Private, Local)

  /** A private[this] parameter accessor */
  final val PrivateLocalParamAccessor: FlagConjunction = allOf(Private, Local, ParamAccessor)

  /** A parameter forwarder */
  final val ParamForwarder: FlagConjunction = allOf(Method, StableRealizable, ParamAccessor)

  /** A private[this] parameter */
  final val PrivateLocalParam: FlagConjunction = allOf(Private, Local, Param)

  /** A private parameter accessor */
  final val PrivateParamAccessor: FlagConjunction = allOf(Private, ParamAccessor)

  /** A local parameter */
  final val ParamAndLocal: FlagConjunction = allOf(Param, Local)

  /** Labeled protected[this] */
  final val ProtectedLocal: FlagConjunction = allOf(Protected, Local)

  final val LiftedMethod: FlagConjunction = allOf(Lifted, Method)

  /** Java symbol which is `protected` and `static` */
  final val StaticProtected: FlagConjunction = allOf(JavaDefined, Protected, JavaStatic)

  final val Scala2Trait: FlagConjunction = allOf(Scala2x, Trait)

  final val AbstractFinal: FlagConjunction = allOf(Abstract, Final)
  final val AbstractSealed: FlagConjunction = allOf(Abstract, Sealed)
  final val AbstractAndOverride: FlagConjunction = allOf(Abstract, Override)

  final val SyntheticArtifact: FlagConjunction = allOf(Synthetic, Artifact)
  final val SyntheticModule: FlagConjunction = allOf(Synthetic, Module)
  final val SyntheticTermParam: FlagConjunction = allOf(Synthetic, TermParam)
  final val SyntheticTypeParam: FlagConjunction = allOf(Synthetic, TypeParam)
  final val SyntheticCase: FlagConjunction = allOf(Synthetic, Case)

  implicit def conjToFlagSet(conj: FlagConjunction): FlagSet =
    FlagSet(conj.bits)
}
