package dotty.tools.dotc
package core

import language.implicitConversions

object Flags {

  object opaques {

    /** A FlagSet represents a set of flags. Flags are encoded as follows:
    *  The first two bits indicate whether a flag set applies to terms,
    *  to types, or to both.  Bits 2..63 are available for properties
    *  and can be doubly used for terms and types.
    */
    opaque type FlagSet = Long
    def FlagSet(bits: Long): FlagSet = bits
    def toBits(fs: FlagSet): Long = fs

    /** A flag set consisting of a single flag */
    opaque type Flag <: FlagSet = Long
    private[Flags] def Flag(bits: Long): Flag = bits
  }
  type FlagSet = opaques.FlagSet
  def FlagSet(bits: Long): FlagSet = opaques.FlagSet(bits)
  // DOTTY TODO: replace previous 2 lines with
  //   export opaques.FlagSet
  // once 0.17 is released and #6721 is in the bootstrap

  type Flag = opaques.Flag

  delegate FlagOps {

    def (x: FlagSet) bits: Long = opaques.toBits(x)

    /** The union of the given flag sets.
     *  Combining two FlagSets with `|` will give a FlagSet
     *  that has the intersection of the applicability to terms/types
     *  of the two flag sets. It is checked that the intersection is not empty.
     */
    def (x: FlagSet) | (y: FlagSet): FlagSet =
      if (x.bits == 0) y
      else if (y.bits == 0) x
      else {
        val tbits = x.bits & y.bits & KINDFLAGS
        if (tbits == 0)
          assert(false, s"illegal flagset combination: $x and $y")
        FlagSet(tbits | ((x.bits | y.bits) & ~KINDFLAGS))
      }

    /** The intersection of the given flag sets */
    def (x: FlagSet) & (y: FlagSet): FlagSet = FlagSet(x.bits & y.bits)

    /** The intersection of a flag set with the complement of another flag set */
    def (x: FlagSet) &~ (y: FlagSet): FlagSet = {
      val tbits = x.bits & KINDFLAGS
      if ((tbits & y.bits) == 0) x
      else FlagSet(tbits | ((x.bits & ~y.bits) & ~KINDFLAGS))
    }

    def (x: FlagSet) ^ (y: FlagSet) =
      FlagSet((x.bits | y.bits) & KINDFLAGS | (x.bits ^ y.bits) & ~KINDFLAGS)

    /** Does the given flag set contain the given flag?
     *  This means that both the kind flags and the carrier bits have non-empty intersection.
     */
    def (x: FlagSet) is (flag: Flag): Boolean = {
      val fs = x.bits & flag.bits
      (fs & KINDFLAGS) != 0 && (fs & ~KINDFLAGS) != 0
    }

    /** Does the given flag set contain the given flag
     *  and at the same time contain none of the flags in the `butNot` set?
     */
    def (x: FlagSet) is (flag: Flag, butNot: FlagSet): Boolean = x.is(flag) && !x.isOneOf(butNot)

    /** Does the given flag set have a non-empty intersection with another flag set?
     *  This means that both the kind flags and the carrier bits have non-empty intersection.
     */
    def (x: FlagSet) isOneOf (flags: FlagSet): Boolean = {
      val fs = x.bits & flags.bits
      (fs & KINDFLAGS) != 0 && (fs & ~KINDFLAGS) != 0
    }

   /** Does the given flag set have a non-empty intersection with another flag set,
    *  and at the same time contain none of the flags in the `butNot` set?
    */
   def (x: FlagSet) isOneOf (flags: FlagSet, butNot: FlagSet): Boolean = x.isOneOf(flags) && !x.isOneOf(butNot)

    /** Does a given flag set have all of the flags of another flag set?
     *  Pre: The intersection of the term/type flags of both sets must be non-empty.
     */
    def (x: FlagSet) isAllOf (flags: FlagSet): Boolean = {
      val fs = x.bits & flags.bits
      ((fs & KINDFLAGS) != 0 || flags.bits == 0) &&
      (fs >>> TYPESHIFT) == (flags.bits >>> TYPESHIFT)
    }

    /** Does a given flag set have all of the flags in another flag set
     *  and at the same time contain none of the flags in the `butNot` set?
     *  Pre: The intersection of the term/type flags of both sets must be non-empty.
     */
    def (x: FlagSet) isAllOf (flags: FlagSet, butNot: FlagSet): Boolean = x.isAllOf(flags) && !x.isOneOf(butNot)

    def (x: FlagSet) isEmpty: Boolean = (x.bits & ~KINDFLAGS) == 0

    /** Is a given flag set a subset of another flag set? */
    def (x: FlagSet) <= (y: FlagSet): Boolean = (x.bits & y.bits) == x.bits

    /** Does the given flag set apply to terms? */
    def (x: FlagSet) isTermFlags: Boolean = (x.bits & TERMS) != 0

    /** Does the given flag set apply to terms? */
    def (x: FlagSet) isTypeFlags: Boolean = (x.bits & TYPES) != 0

    /** The given flag set with all flags transposed to be type flags */
    def (x: FlagSet) toTypeFlags: FlagSet = if (x.bits == 0) x else FlagSet(x.bits & ~KINDFLAGS | TYPES)

    /** The given flag set with all flags transposed to be term flags */
    def (x: FlagSet) toTermFlags: FlagSet = if (x.bits == 0) x else FlagSet(x.bits & ~KINDFLAGS | TERMS)

    /** The given flag set with all flags transposed to be common flags */
    def (x: FlagSet) toCommonFlags: FlagSet = if (x.bits == 0) x else FlagSet(x.bits | KINDFLAGS)

    /** The number of non-kind flags in the given flag set */
    def (x: FlagSet) numFlags: Int = java.lang.Long.bitCount(x.bits & ~KINDFLAGS)

    /** The lowest non-kind bit set in the given flag set */
    def (x: FlagSet) firstBit: Int = java.lang.Long.numberOfTrailingZeros(x.bits & ~KINDFLAGS)

    /** The  list of non-empty names of flags with given index idx that are set in the given flag set */
    private def (x: FlagSet) flagString(idx: Int): List[String] =
      if ((x.bits & (1L << idx)) == 0) Nil
      else {
        def halfString(kind: Int) =
          if ((x.bits & (1L << kind)) != 0) flagName(idx)(kind) else ""
        val termFS = halfString(TERMindex)
        val typeFS = halfString(TYPEindex)
        val strs = termFS :: (if (termFS == typeFS) Nil else typeFS :: Nil)
        strs filter (_.nonEmpty)
      }

    /** The list of non-empty names of flags that are set in teh given flag set */
    def (x: FlagSet) flagStrings(privateWithin: String): Seq[String] = {
      var rawStrings = (2 to MaxFlag).flatMap(x.flagString(_)) // DOTTY problem: cannot drop with (_)
      if (!privateWithin.isEmpty && !x.is(Protected))
      	rawStrings = rawStrings :+ "private"
      val scopeStr = if (x.is(Local)) "this" else privateWithin
      if (scopeStr != "")
        rawStrings.filter(_ != "<local>").map {
          case "private" => s"private[$scopeStr]"
          case "protected" => s"protected[$scopeStr]"
          case str => str
        }
      else rawStrings
    }

    /** The string representation of the given flag set */
    def (x: FlagSet) flagsString: String = x.flagStrings("").mkString(" ")
  }

  def termFlagSet(x: Long) = FlagSet(TERMS | x)

  private inline val TYPESHIFT = 2
  private inline val TERMindex = 0
  private inline val TYPEindex = 1
  private inline val TERMS = 1 << TERMindex
  private inline val TYPES = 1 << TYPEindex
  private inline val KINDFLAGS = TERMS | TYPES

  private inline val FirstFlag = 2
  private inline val FirstNotPickledFlag = 48
  private inline val MaxFlag = 63

  private val flagName = Array.fill(64, 2)("")

  private def isDefinedAsFlag(idx: Int) = flagName(idx).exists(_.nonEmpty)

  /** The flag set containing all defined flags of either kind whose bits
   *  lie in the given range
   */
  private def flagRange(start: Int, end: Int) =
    FlagSet((KINDFLAGS.toLong /: (start until end)) ((bits, idx) =>
      if (isDefinedAsFlag(idx)) bits | (1L << idx) else bits))

  /** The union of all flags in given flag set */
  def union(flagss: FlagSet*): FlagSet = {
    var flag = EmptyFlags
    for (f <- flagss)
      flag |= f
    flag
  }

  def commonFlags(flagss: FlagSet*): FlagSet = union(flagss.map(_.toCommonFlags): _*)

  /** The empty flag set */
  val EmptyFlags: FlagSet = FlagSet(0)

  /** The undefined flag set */
  val UndefinedFlags: FlagSet = FlagSet(~KINDFLAGS)

  /** Three flags with given index between 2 and 63.
   *  The first applies to both terms and types. the second is a term flag, and
   *  the third is a type flag. Installs given name(s) as the name(s) of the flags.
   *  @param name     The name to be used for the term flag
   *  @param typeName The name to be used for the type flag, if it is different from `name`.
   */
  private def newFlags(index: Int, name: String, typeName: String = ""): (Flag, Flag, Flag) = {
    flagName(index)(TERMindex) = name
    flagName(index)(TYPEindex) = if (typeName.isEmpty) name else typeName
    val bits = 1L << index
    (opaques.Flag(KINDFLAGS | bits), opaques.Flag(TERMS | bits), opaques.Flag(TYPES | bits))
  }

  // ----------------- Available flags -----------------------------------------------------

  /** Labeled with `private` modifier */
  val (Private @ _, PrivateTerm @ _, PrivateType @ _) = newFlags(2, "private")

  /** Labeled with `protected` modifier */
  val (Protected @ _, _, _) = newFlags(3, "protected")

  /** Labeled with `override` modifier */
  val (Override @ _, _, _) = newFlags(4, "override")

  /** A declared, but not defined member */
  val (Deferred @ _, DeferredTerm @ _, DeferredType @ _) = newFlags(5, "<deferred>")

  /** Labeled with `final` modifier */
  val (Final @ _, _, _) = newFlags(6, "final")

  /** A method symbol */
  val (_, Method @ _, HigherKinded @ _) = newFlags(7, "<method>", "<higher kinded>") // TODO drop HigherKinded

  /** A (term or type) parameter to a class or method */
  val (Param @ _, TermParam @ _, TypeParam @ _) = newFlags(8, "<param>")

  /** Labeled with `implicit` modifier (implicit value) */
  val (Implicit @ _, ImplicitTerm @ _, _) = newFlags(9, "implicit")

  /** Labeled with `lazy` (a lazy val) / a trait */
  val (LazyOrTrait @ _, Lazy @ _, Trait @ _) = newFlags(10, "lazy", "<trait>")

  /** A value or variable accessor (getter or setter) */
  val (AccessorOrSealed @ _, Accessor @ _, Sealed @ _) = newFlags(11, "<accessor>", "sealed")

  /** A mutable var */
  val (_, Mutable @ _, _) = newFlags(12, "mutable")

  /** Symbol is local to current class (i.e. private[this] or protected[this]
   *  pre: Private or Protected are also set
   */
  val (Local @ _, _, _) = newFlags(13, "<local>")

  /** A field generated for a primary constructor parameter (no matter if it's a 'val' or not),
   *  or an accessor of such a field.
   */
  val (_, ParamAccessor @ _, _) = newFlags(14, "<paramaccessor>")

  /** A value or class implementing a module */
  val (Module @ _, ModuleVal @ _, ModuleClass @ _) = newFlags(15, "module")

   /** A value or class representing a package */
  val (Package @ _, PackageVal @ _, PackageClass @ _) = newFlags(16, "<package>")

  /** A case class or its companion object
   *  Note: Case is also used to indicate that a symbol is bound by a pattern.
   */
  val (Case @ _, CaseVal @ _, CaseClass @ _) = newFlags(17, "case")

  /** A compiler-generated symbol, which is visible for type-checking
   *  (compare with artifact)
   */
  val (Synthetic @ _, _, _) = newFlags(18, "<synthetic>")

  /** Labelled with `inline` modifier */
  val (Inline @ _, _, _) = newFlags(19, "inline")

  /** An outer accessor / a covariant type variable */
  val (OuterOrCovariant @ _, OuterAccessor @ _, Covariant @ _) = newFlags(20, "<outer accessor>", "<covariant>")

  /** The label of a labeled block / a contravariant type variable */
  val (LabelOrContravariant @ _, Label @ _, Contravariant @ _) = newFlags(21, "<label>", "<contravariant>")

  /** Labeled with of abstract & override
   *    /
   *  A trait that has only abstract methods as members
   *  and therefore can be represented by a Java interface.
   *  Warning: PureInterface is set during regular typer pass, should be tested only after typer.
   */
  val (_, AbsOverride @ _, PureInterface @ _) = newFlags(22, "abstract override", "interface")

  /** Labeled with `abstract` modifier (an abstract class)
   *  Note: You should never see Abstract on any symbol except a class.
   *  Note: the flag counts as common, because it can be combined with OVERRIDE in a term.
   */
  val (Abstract @ _, _, _) = newFlags(23, "abstract")

  /** Lazy val or method is known or assumed to be stable and realizable */
  val (_, StableRealizable @ _, _) = newFlags(24, "<stable>")

  /** A case parameter accessor */
  val (_, CaseAccessor @ _, _) = newFlags(25, "<caseaccessor>")

  /** A Scala 2x super accessor / an unpickled Scala 2.x class */
  val (SuperAccessorOrScala2x @ _, Scala2SuperAccessor @ _, Scala2x @ _) = newFlags(26, "<superaccessor>", "<scala-2.x>")

  /** A method that has default params */
  val (_, DefaultParameterized @ _, _) = newFlags(27, "<defaultparam>")

  /** An extension method */
  val (_, Extension @ _, _) = newFlags(28, "<extension>")

  /** An inferable (`given`) parameter */
  val (Given @ _, _, _) = newFlags(29, "given")

  /** Symbol is defined by a Java class */
  val (JavaDefined @ _, _, _) = newFlags(30, "<java>")

  /** Symbol is implemented as a Java static */
  val (JavaStatic @ _, JavaStaticTerm @ _, JavaStaticType @ _) = newFlags(31, "<static>")

  /** Variable is accessed from nested function
   *    /
   *  Trait does not have fields or initialization code.
   *  Warning: NoInits is set during regular typer pass, should be tested only after typer.
   */
  val (_, Captured @ _, NoInits @ _) = newFlags(32, "<captured>", "<noinits>")

  /** Symbol should be ignored when typechecking; will be marked ACC_SYNTHETIC in bytecode */
  val (Artifact @ _, _, _) = newFlags(33, "<artifact>")

  /** A bridge method. Set by Erasure */
  val (_, Bridge @ _, _) = newFlags(34, "<bridge>")

  /** A proxy for an argument to an inline method */
  val (_, InlineProxy @ _, _) = newFlags(35, "<inline proxy>")

  /** Symbol is a method which should be marked ACC_SYNCHRONIZED */
  val (_, Synchronized @ _, _) = newFlags(36, "<synchronized>")

  /** Symbol is a Java-style varargs method */
  val (_, JavaVarargs @ _, _) = newFlags(37, "<varargs>")

  /** Symbol is a Java default method */
  val (_, DefaultMethod @ _, _) = newFlags(38, "<defaultmethod>")

  val (Delegate @ _, _, _) = newFlags(39, "delegate")

  /** Symbol is an enum class or enum case (if used with case) */
  val (Enum @ _, _, _) = newFlags(40, "<enum>")

  /** An export forwarder */
  val (Exported @ _, _, _) = newFlags(41, "exported")

  /** Labeled with `erased` modifier (erased value)  */
  val (_, Erased @ _, _) = newFlags(42, "erased")

  /** An opaque type alias or a class containing one */
  val (Opaque @ _, _, _) = newFlags(43, "opaque")


  // ------------ Flags following this one are not pickled ----------------------------------

  /** Symbol is not a member of its owner */
  val (NonMember @ _, _, _) = newFlags(45, "<non-member>")

  /** Denotation is in train of being loaded and completed, used to catch cyclic dependencies */
  val (Touched @ _, _, _) = newFlags(48, "<touched>")

  /** Class has been lifted out to package level, local value has been lifted out to class level */
  val (Lifted @ _, _, _) = newFlags(51, "<lifted>")

  /** Term member has been mixed in */
  val (MixedIn @ _, _, _) = newFlags(52, "<mixedin>")

  /** Symbol is a generated specialized member */
  val (Specialized @ _, _, _) = newFlags(53, "<specialized>")

  /** Symbol is a self name */
  val (_, SelfName @ _, _) = newFlags(54, "<selfname>")

  /** An existentially bound symbol (Scala 2.x only) */
  val (Scala2ExistentialCommon @ _, _, Scala2Existential @ _) = newFlags(55, "<existential>")

  /** Children were queried on this class */
  val (_, _, ChildrenQueried @ _) = newFlags(56, "<children-queried>")

  /** A module variable (Scala 2.x only)
   *    /
   *  A Scala 2.x trait that has been partially augmented.
   *  This is set in `AugmentScala2Trait` and reset in `LinkScala2Impls`
   *  when the trait is fully augmented.
   */
  val (_, Scala2ModuleVar @ _, Scala2xPartiallyAugmented @ _) = newFlags(57, "<modulevar>", "<scala-2.x-partially-augmented>")

  /** A macro */
  val (Macro @ _, _, _) = newFlags(58, "<macro>")

  /** Translation of Scala2's EXPANDEDNAME flag. This flag is never stored in
   *  symbols, is only used locally when reading the flags of a Scala2 symbol.
   *  It's therefore safe to share the code with `InheritedDefaultParams` because
   *  the latter is never present in Scala2 unpickle info.
   *    /
   *  A method that is known to have inherited default parameters
   */
  val (Scala2ExpandedName @ _, InheritedDefaultParams @ _, _) = newFlags(59, "<inherited-default-param>")

  /** A method that is known to have no default parameters
   *    /
   *  A type symbol with provisional empty bounds
   */
  val (_, NoDefaultParams @ _, Provisional @ _) = newFlags(60, "<no-default-param>", "<provisional>")

  /** A denotation that is valid in all run-ids */
  val (Permanent @ _, _, _) = newFlags(61, "<permanent>")

// --------- Combined Flag Sets and Conjunctions ----------------------

  /** All possible flags */
  val AnyFlags: FlagSet = flagRange(FirstFlag, MaxFlag)

  /** These flags are pickled */
  val PickledFlags: FlagSet = flagRange(FirstFlag, FirstNotPickledFlag)

  /** Flags representing access rights */
  val AccessFlags: FlagSet = Local | Private | Protected

  /** Flags representing source modifiers */
  private val CommonSourceModifierFlags: FlagSet =
    commonFlags(Private, Protected, Final, Case, Implicit, Delegate, Given, Override, JavaStatic)

  val TypeSourceModifierFlags: FlagSet =
    CommonSourceModifierFlags.toTypeFlags | Abstract | Sealed | Opaque

  val TermSourceModifierFlags: FlagSet =
    CommonSourceModifierFlags.toTermFlags | Inline | AbsOverride | Lazy | Erased

  /** Flags representing modifiers that can appear in trees */
  val ModifierFlags: FlagSet =
    TypeSourceModifierFlags.toCommonFlags |
    TermSourceModifierFlags.toCommonFlags |
    commonFlags(Module, Param, Synthetic, Package, Local, Mutable, Trait)

  /** Flags that are not (re)set when completing the denotation
   *  TODO: Should check that FromStartFlags do not change in completion
   */
  val FromStartFlags: FlagSet = commonFlags(
    Module, Package, Deferred, Method, Case,
    HigherKinded, Param, ParamAccessor,
    Scala2ExistentialCommon, Mutable, Opaque, Touched, JavaStatic,
    OuterOrCovariant, LabelOrContravariant, CaseAccessor,
    Extension, NonMember, Implicit, Given, Delegate, Permanent, Synthetic,
    SuperAccessorOrScala2x, Inline)

  /** Flags that are not (re)set when completing the denotation, or, if symbol is
   *  a top-level class or object, when completing the denotation once the class
   *  file defining the symbol is loaded (which is generally before the denotation
   *  is completed)
   */
  val AfterLoadFlags: FlagSet = commonFlags(
    FromStartFlags, AccessFlags, Final, AccessorOrSealed, LazyOrTrait, SelfName)


  /** A value that's unstable unless complemented with a Stable flag */
  val UnstableValueFlags: FlagSet = Mutable | Method

  /** Flags that express the variance of a type parameter. */
  val VarianceFlags: FlagSet = Covariant | Contravariant

// ----- Creation flag sets ----------------------------------

  /** Modules always have these flags set */
  val ModuleValCreationFlags: FlagSet = ModuleVal | Lazy | Final | StableRealizable

  /** Module classes always have these flags set */
  val ModuleClassCreationFlags: FlagSet = ModuleClass | Final

  /** Accessors always have these flags set */
  val AccessorCreationFlags: FlagSet = Method | Accessor

  /** Pure interfaces always have these flags */
  val PureInterfaceCreationFlags: FlagSet = Trait | NoInits | PureInterface

  /** The flags of the self symbol */
  val SelfSymFlags: FlagSet = Private | Local | Deferred

  /** The flags of a class type parameter */
  val ClassTypeParamCreationFlags: FlagSet =
    TypeParam | Deferred | Private | Local

  /** Packages and package classes always have these flags set */
  val PackageCreationFlags: FlagSet =
    Module | Package | Final | JavaDefined

// ----- Retained flag sets ----------------------------------

  /** Flags that are passed from a type parameter of a class to a refinement symbol
    * that sets the type parameter */
  val RetainedTypeArgFlags: FlagSet = VarianceFlags | Protected | Local

  /** Flags that can apply to both a module val and a module class, except those that
    *  are added at creation anyway
    */
  val RetainedModuleValAndClassFlags: FlagSet =
    AccessFlags | Package | Case |
    Synthetic | JavaDefined | JavaStatic | Artifact |
    Lifted | MixedIn | Specialized

  /** Flags that can apply to a module val */
  val RetainedModuleValFlags: FlagSet = RetainedModuleValAndClassFlags |
    Override | Final | Method | Implicit | Delegate | Lazy |
    Accessor | AbsOverride | StableRealizable | Captured | Synchronized | Erased

  /** Flags that can apply to a module class */
  val RetainedModuleClassFlags: FlagSet = RetainedModuleValAndClassFlags | Enum

  /** Flags retained in export forwarders */
  val RetainedExportFlags = Delegate | Given | Implicit | Extension

// ------- Other flag sets -------------------------------------

  val AbstractFinal: FlagSet                 = Abstract | Final
  val AbstractOverride: FlagSet              = Abstract | Override
  val AbstractSealed: FlagSet                = Abstract | Sealed
  val AbstractOrTrait: FlagSet               = Abstract | Trait
  val PrivateAccessor: FlagSet               = Accessor | Private
  val AccessorOrSynthetic: FlagSet           = Accessor | Synthetic
  val EnumCase: FlagSet                      = Case | Enum
  val CovariantLocal: FlagSet                = Covariant | Local                              // A covariant type parameter
  val ContravariantLocal: FlagSet            = Contravariant | Local                          // A contravariant type parameter
  val HasDefaultParamsFlags: FlagSet         = DefaultParameterized | InheritedDefaultParams  // Has defined or inherited default parameters
  val DefaultParameter: FlagSet              = DefaultParameterized | Param                   // A Scala 2x default parameter
  val DeferredOrLazy: FlagSet                = Deferred | Lazy
  val DeferredOrLazyOrMethod: FlagSet        = Deferred | Lazy | Method
  val DeferredOrTermParamOrAccessor: FlagSet = Deferred | ParamAccessor | TermParam           // term symbols without right-hand sides
  val DeferredOrTypeParam: FlagSet           = Deferred | TypeParam                           // type symbols without right-hand sides
  val DelegateOrGiven: FlagSet               = Delegate | Given
  val DelegateOrGivenOrImplicit: FlagSet     = Delegate | Given | Implicit
  val DelegateOrGivenOrImplicitVal: FlagSet  = DelegateOrGivenOrImplicit.toTermFlags
  val DelegateOrImplicit: FlagSet            = Delegate | Implicit
  val EnumValue: FlagSet                     = Enum | JavaStatic | StableRealizable           // A Scala enum value
  val StableOrErased: FlagSet                = Erased | StableRealizable                      // Assumed to be pure
  val ExtensionMethod: FlagSet               = Extension | Method
  val FinalOrInline: FlagSet                 = Final | Inline
  val FinalOrModuleClass: FlagSet            = Final | ModuleClass                            // A module class or a final class
  val EffectivelyFinalFlags: FlagSet         = Final | Private
  val FinalOrSealed: FlagSet                 = Final | Sealed
  val GivenOrImplicit: FlagSet               = Given | Implicit
  val InlineOrProxy: FlagSet                 = Inline | InlineProxy                           // An inline method or inline argument proxy */
  val InlineMethod: FlagSet                  = Inline | Method
  val InlineParam: FlagSet                   = Inline | Param
  val InlineByNameProxy: FlagSet             = InlineProxy | Method
  val JavaEnumTrait: FlagSet                 = JavaDefined | Enum                             // A Java enum trait
  val JavaEnumValue: FlagSet                 = JavaDefined | EnumValue                        // A Java enum value
  val StaticProtected: FlagSet               = JavaDefined | JavaStatic | Protected           // Java symbol which is `protected` and `static`
  val JavaModule: FlagSet                    = JavaDefined | Module                           // A Java companion object
  val JavaInterface: FlagSet                 = JavaDefined | NoInits | Trait
  val JavaProtected: FlagSet                 = JavaDefined | Protected
  val MethodOrLazy: FlagSet                  = Lazy | Method
  val MutableOrLazy: FlagSet                 = Lazy | Mutable
  val LiftedMethod: FlagSet                  = Lifted | Method
  val LocalParam: FlagSet                    = Local | Param
  val LocalParamAccessor: FlagSet            = Local | ParamAccessor | Private
  val PrivateLocal: FlagSet                  = Local | Private                                // private[this]
  val ProtectedLocal: FlagSet                = Local | Protected
  val MethodOrModule: FlagSet                = Method | Module
  val ParamForwarder: FlagSet                = Method | ParamAccessor | StableRealizable      // A parameter forwarder
  val PrivateMethod: FlagSet                 = Method | Private
  val NoInitsInterface: FlagSet              = NoInits | PureInterface
  val NoInitsTrait: FlagSet                  = NoInits | Trait                                // A trait that does not need to be initialized
  val ValidForeverFlags: FlagSet             = Package | Permanent | Scala2ExistentialCommon
  val TermParamOrAccessor: FlagSet           = Param | ParamAccessor
  val PrivateParamAccessor: FlagSet          = ParamAccessor | Private
  val PrivateOrSynthetic: FlagSet            = Private | Synthetic
  val ClassTypeParam: FlagSet                = Private | TypeParam
  val Scala2Trait: FlagSet                   = Scala2x | Trait
  val SyntheticArtifact: FlagSet             = Synthetic | Artifact
  val SyntheticCase: FlagSet                 = Synthetic | Case
  val SyntheticDelegateMethod: FlagSet       = Synthetic | Delegate | Method
  val SyntheticModule: FlagSet               = Synthetic | Module
  val SyntheticOpaque: FlagSet               = Synthetic | Opaque
  val SyntheticTermParam: FlagSet            = Synthetic | TermParam
  val SyntheticTypeParam: FlagSet            = Synthetic | TypeParam
}
