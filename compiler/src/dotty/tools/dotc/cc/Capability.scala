package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Decorators.*
import util.{SimpleIdentitySet, EqHashMap}
import util.common.alwaysTrue
import scala.collection.mutable
import CCState.*
import Periods.{NoRunId, RunWidth}
import compiletime.uninitialized
import StdNames.nme
import CaptureSet.{Refs, emptyRefs, VarState}
import Annotations.Annotation
import Flags.*
import config.Printers.capt
import annotation.constructorOnly
import ast.tpd
import printing.{Printer, Showable}
import printing.Texts.Text
import reporting.{Message, trace}
import NameOps.isImpureFunction
import annotation.internal.sharable
import collection.immutable

/** Capabilities are members of capture sets. They partially overlap with types
 *  as shown in the trait hierarchy below.
 *
 *  Capability --+-- RootCapabilty -----+-- GlobalCap  --------+-- GlobalAny
 *               |                      |                      +-- GlobalFresh
 *               |                      +-- LocalCap
 *               |                      +-- ResultCap
 *               |
 *               +-- CoreCapability ----+-- ObjectCapability --+-- TermRef
 *               |                      |                      +-- ThisType
 *               |                      |                      +-- TermParamRef
 *               |                      |
 *               |                      +-- SetCapability -----+-- TypeRef
 *               |                                             +-- TypeParamRef
 *               |
 *               +-- DerivedCapability -+-- Reach
 *                                      +-- Only
 *                                      +-- ReadOnly
 *                                      +-- Maybe
 *
 *  All CoreCapabilities are Types, or, more specifically instances of TypeProxy.
 */
object Capabilities:
  opaque type Validity = Int
  def validId(runId: Int, iterId: Int): Validity =
    runId + (iterId << RunWidth)
  def currentId(using Context): Validity = validId(ctx.runId, ccState.iterationId)
  val invalid: Validity = validId(NoRunId, 0)

  @sharable private var nextRootId = 0

  /** The base trait of all root capabilities */
  trait RootCapability extends Capability:
    val rootId = nextRootId
    nextRootId += 1
    def descr(using Context): String

  /** The base trait of all capabilities represented as types */
  trait CoreCapability extends TypeProxy, Capability:
    override def toText(printer: Printer): Text = printer.toText(this)

  trait ObjectCapability extends CoreCapability

  trait SetCapability extends CoreCapability

  trait DerivedCapability extends Capability:
    def underlying: Capability

  /** If `x` is a capability, its maybe capability `x?`. `x?` stands for a capability
   *  `x` that might or might not be part of a capture set. We have `{} <: {x?} <: {x}`.
   *  Maybe capabilities cannot be propagated between sets. If `a <: b` and `a`
   *  acquires `x?` then `x` is propagated to `b` as a conservative approximation.
   *
   *  Maybe capabilities should only arise for capture sets that appear in invariant
   *  position in their surrounding type. They are similar to TypeBounds types, but
   *  restricted to capture sets. For instance,
   *
   *      Array[C^{x?}]
   *
   *  should be morally equivalent to
   *
   *      Array[_ >: C^{} <: C^{x}]
   *
   *   but it has fewer issues with type inference.
   */
  case class Maybe(underlying: Capability) extends DerivedCapability

  /** The readonly capability `x.rd`. We have {x.rd} <: {x}.
   *
   *  Read-only capabilities cannot wrap maybe capabilities
   *  but they can wrap reach capabilities. We have
   *      (x?).readOnly = (x.rd)?
   */
  case class ReadOnly(underlying: CoreCapability | RootCapability | Reach | Restricted)
  extends DerivedCapability

  /** The restricted capability `x.only[C]`. We have {x.only[C]} <: {x}.
   *
   *  Restricted capabilities cannot wrap maybe capabilities or read-only capabilities
   *  but they can wrap reach capabilities. We have
   *      (x?).restrict[T] = (x.restrict[T])?
   *      (x.rd).restrict[T] = (x.restrict[T]).rd
   */
  case class Restricted(underlying: CoreCapability | RootCapability | Reach, cls: ClassSymbol)
  extends DerivedCapability

  /** If `x` is a capability, its reach capability `x*`. `x*` stands for all
   *  capabilities reachable through `x`.
   *  We have `{x} <: {x*} <: dcs(x)}` where the deep capture set `dcs(x)` of `x`
   *  is the union of all capture sets that appear in covariant position in the
   *  type of `x`. If `x` and `y` are different variables then `{x*}` and `{y*}`
   *  are unrelated.
   *
   *  Reach capabilities cannot wrap read-only capabilities or maybe capabilities.
   *  We have
   *      (x?).reach        = (x.reach)?
   *      (x.rd).reach      = (x.reach).rd
   *      (x.only[T]).reach = (x*).only[T]
   */
  case class Reach(underlying: ObjectCapability) extends DerivedCapability

  /** A class for the global root capabilities referenced as `caps.any` and `caps.fresh`.
   *  They do not subsume other capabilities, except in arguments of `withCapAsRoot` calls.
   */
  class GlobalCap(val fullName: String) extends RootCapability:
    def descr(using Context) = s"the root capability $fullName"
    override val maybe = Maybe(this)
    override val readOnly = ReadOnly(this)
    override def restrict(cls: ClassSymbol)(using Context) = Restricted(this, cls)
    override def reach = unsupported("caps.any.reach")
    override def singletonCaptureSet(using Context) = CaptureSet.universal
    override def captureSetOfInfo(using Context) = singletonCaptureSet
    override def cached[C <: DerivedCapability](newRef: C): C = unsupported("cached")
    override def invalidateCaches() = ()

  /** The global root capability referenced as `caps.any` */
  @sharable // We override in GlobalCap all operations that access internal capability state
  object GlobalAny extends GlobalCap("caps.any")

  /** The global root capability referenced as `caps.fresh` */
  @sharable // We override in GlobalCap all operations that access internal capability state
  object GlobalFresh extends GlobalCap("caps.fresh")

  /** The class of local roots named "any". These do subsume other capabilties in scope.
   *  They track with hidden sets which other capabilities were subsumed.
   *  Hidden sets are inspected by separation checking.
   *  @param owner   the owner of the context in which the LocalCap was created
   *  @param origin  an indication where and why the LocalCap was created, used
   *                 for diagnostics
   */
  case class LocalCap(val prefix: Type)
      (val owner: Symbol, val origin: Origin, origHidden: CaptureSet.HiddenSet | Null)
      (using @constructorOnly ctx: Context)
  extends RootCapability:
    val hiddenSet =
      if origHidden == null then CaptureSet.HiddenSet(owner, this: @unchecked)
      else origHidden
      // fails initialization check without the @unchecked

    def derivedLocalCap(newPrefix: Type)(using Context): LocalCap =
      if newPrefix eq prefix then this
      else if newPrefix eq hiddenSet.owningCap.prefix then
        hiddenSet.owningCap
      else
        hiddenSet.derivedCaps
          .getOrElseUpdate(newPrefix, LocalCap(newPrefix)(owner, origin, hiddenSet))

    /** A map from context owners to skolem TermRefs that were created by ensurePath
     *  TypeMap's mapCapability.
     */
    var skolems: immutable.Map[Symbol, TermRef] = immutable.HashMap.empty

    //assert(rootId != 4, i"any $prefix, $origin, ${ctx.owner}")

    /** Is this LocalCap (definitely) classified? If that's the case, the
     *  classifier cannot be changed anymore.
     *  We need to distinguish LocalCaps that can still be classified from
     *  ones that cannot. Once a LocalCap is part of a constant capture set,
     *  it gets classified by the type that prefixes the set and that classification
     *  cannot be changed anymore. But other LocalCaps are created as members of
     *  variable sets and then their classification status is open and can be
     *  constrained further.
     */
    private[Capabilities] var isClassified = false

    override def equals(that: Any) = that match
      case that: LocalCap => this eq that
      case _ => false

    /** Is this LocalCap at the right level to be able to subsume `ref`?
     */
    def acceptsLevelOf(ref: Capability)(using Context): Boolean =
      if ccConfig.useLocalCapLevels && !CCState.collapseLocalCaps then
        val refOwner = ref.levelOwner
        ccOwner.isContainedIn(refOwner)
        || classifier.derivesFrom(defn.Caps_Unscoped)
      else ref.core match
        case ResultCap(_) | _: ParamRef => false
        case _ => true

    /** Classify this LocalCap as `cls`, provided `isClassified` is still false.
     *  @param  freeze  Determines future `isClassified` state.
     */
    def adoptClassifier(cls: ClassSymbol, freeze: Boolean)(using Context): Unit =
      if !isClassified then
        hiddenSet.adoptClassifier(cls)
        if freeze then isClassified = true

    def ccOwnerStr(using Context): String =
      val owner = ccOwner
      if owner.name == nme.SKOLEM then i"a new instance of ${hiddenSet.owner}"
      else owner.show

    def descr(using Context) =
      val originStr = origin match
        case Origin.InDecl(sym, _) if sym.exists =>
          origin.explanation
        case _ =>
          i" created in ${hiddenSet.owner.sanitizedDescription}${origin.explanation}"
      val classifierStr =
        if hiddenSet.classifier != defn.AnyClass
        then i" classified as ${hiddenSet.classifier.name}"
        else ""
      i"a root capability$classifierStr$originStr"

  object LocalCap:
    def apply(owner: Symbol, prefix: Type, origin: Origin)(using Context): LocalCap =
      new LocalCap(prefix)(owner, origin, null)
    def apply(owner: Symbol, origin: Origin)(using Context): LocalCap =
      apply(owner, owner.skipStrictValDef.thisType, origin)
    def apply(origin: Origin)(using Context): LocalCap =
      apply(ctx.owner, origin)

  /** A root capability associated with a function type. These are conceptually
   *  existentially quantified over the function's result type.
   *  @param  binder  The function type with which the capability is associated.
   *                  It is a MethodicType since we also have ResultCaps that are
   *                  associated with the ExprTypes of parameterless functions.
   *                  Currently we never create results over PolyTypes since a PolyType
   *                  used as a type (not a method info) is always followed by a MethodType.
   * Setup:
   *
   *  In the setup phase, `fresh` instances in the result of a dependent function type
   *  or method type such as `(x: T): C^{fresh}` are converted to `ResultCap(binder)` instances,
   *  where `binder` refers to the immediately enclosing method type.
   *
   *  If one needs to refer to an outer method type as the binder instead, one can achieve that
   *  by using a type alias. For instance:
   *
   *      type F[X^] = (x: A) => C^{X}
   *      () => F[{fresh}]
   *
   *  With explicit quantification, this would be equivalent to
   *
   *      () => \exists fresh. (x: A) => C^{fresh}
   *
   *  The trick is that the argument `fresh` is bound before the `F` alias is expanded.
   */
  case class ResultCap(binder: MethodicType) extends RootCapability:

    private var myOrigin: RootCapability = GlobalAny
    private var variants: SimpleIdentitySet[ResultCap] = SimpleIdentitySet.empty

    /** Every ResultCap capability has an origin. This is
     *   - A LocalCap capability `f`, if the current capability was created as a mirror
     *     of `f` in the ToResult map.
     *   - Another ResultCap capability `r`, if the current capability was created
     *     via a chain of `derivedResult` calls from an original ResultCap `r`
     *     (which was not created using `derivedResult`).
     *   - GlobalAny otherwise
     */
    def origin: RootCapability = myOrigin

    /** Initialize origin of this capability to a LocalCap instance (or to GlobalCap
     *  if separation checks are turned off).
     *  @pre The capability's origin was not yet set.
     */
    def setOrigin(localCapOrigin: LocalCap | GlobalCap): this.type =
      assert(myOrigin.isInstanceOf[GlobalCap])
      myOrigin = localCapOrigin
      this

    /** If the current capability was created via a chain of `derivedResult` calls
     *  from an original ResultCap `r`, that `r`. Otherwise `this`.
     */
    def primaryResultCap: ResultCap = origin match
      case origin: ResultCap => origin
      case _ => this

    def originalBinder: MethodicType = primaryResultCap.binder

    /** A ResultCap with given `binder1` derived from this capability.
     *  This is typically done as a result of a SubstBinding map.
     *  ResultCaps so created are cached, so that for every pair
     *  of a ResultCap `r` and a binder `b`, there exists at most one ResultCap
     *  instance that is derived transitively from `r` and has binder `b`.
     */
    def derivedResult(binder1: MethodicType): ResultCap =
      if binder1 eq binder then this
      else
        val primary = primaryResultCap
        primary.variants.iterator.find(_.binder eq binder1) match
          case Some(rcap) => rcap
          case None =>
            val rcap = ResultCap(binder1)
            rcap.myOrigin = primary
            primary.variants += rcap
            rcap

    def descr(using Context) =
      i"a root capability associated with the result type of $binder"
  end ResultCap

  /** A trait for references in CaptureSets. These can be NamedTypes, ThisTypes or ParamRefs,
   *  as well as three kinds of AnnotatedTypes representing readOnly, reach, and maybe capabilities.
   *  If there are several annotations they come with an order:
   *  `*` first, `.only` next, `.rd` next, `?` last.
   */
  trait Capability extends Showable:

    private var myCaptureSet: CaptureSet | Null = uninitialized
    private var captureSetValid: Validity = invalid
    private var mySingletonCaptureSet: CaptureSet.Const | Null = null
    private var myDerived: List[DerivedCapability] = Nil
    private var myClassifiers: Classifiers = UnknownClassifier
    private var classifiersValid: Validity = invalid

    protected def cached[C <: DerivedCapability](newRef: C): C =
      def recur(refs: List[DerivedCapability]): C = refs match
        case ref :: refs1 =>
          val exists = ref match
            case Restricted(_, cls) =>
              newRef match
                case Restricted(_, newCls) => cls == newCls
                case _ => false
            case _ =>
              ref.getClass == newRef.getClass
          if exists then ref.asInstanceOf[C]
          else recur(refs1)
        case Nil =>
          myDerived = newRef :: myDerived
          newRef
      recur(myDerived)

    def maybe: Maybe = this match
      case self: Maybe => self
      case _ => cached(Maybe(this))

    def readOnly: ReadOnly | Maybe = this match
      case Maybe(ref1) => Maybe(ref1.readOnly)
      case self: ReadOnly => self
      case self: (CoreCapability | RootCapability | Reach | Restricted) => cached(ReadOnly(self))

    def restrict(cls: ClassSymbol)(using Context): Restricted | ReadOnly | Maybe = this match
      case Maybe(ref1) => Maybe(ref1.restrict(cls))
      case ReadOnly(ref1) => ReadOnly(ref1.restrict(cls).asInstanceOf[Restricted])
      case self @ Restricted(ref1, prevCls) =>
        val combinedCls = leastClassifier(prevCls, cls)
        if combinedCls == prevCls then self
        else cached(Restricted(ref1, combinedCls))
      case self: (CoreCapability | RootCapability | Reach) => cached(Restricted(self, cls))

    def reach: Reach | Restricted | ReadOnly | Maybe = this match
      case Maybe(ref1) => Maybe(ref1.reach)
      case ReadOnly(ref1) => ReadOnly(ref1.reach.asInstanceOf[Reach | Restricted])
      case Restricted(ref1, cls) => Restricted(ref1.reach.asInstanceOf[Reach], cls)
      case self: Reach => self
      case self: ObjectCapability => cached(Reach(self))

    /** Is this a maybe reference of the form `x?`? */
    final def isMaybe(using Context): Boolean = this ne stripMaybe

    /** Is this a read-only reference of the form `x.rd` or `x.rd?` or a
     *  capture set variable with only read-ony references in its upper bound?
     */
    final def isReadOnly(using Context): Boolean = this match
      case tp: SetCapability => tp.captureSetOfInfo.isReadOnly
      case _ => this ne stripReadOnly

    /** The classifier, either given in an explicit `.only` or assumed for a
     *  LocalCap. AnyRef for unclassified LocalCaps. Otherwise NoSymbol if no
     *  classifier is given.
     */
    final def classifier(using Context): Symbol = this match
      case Restricted(_, cls) => cls
      case ReadOnly(ref1) => ref1.classifier
      case Maybe(ref1) => ref1.classifier
      case self: LocalCap => self.hiddenSet.classifier
      case _ => NoSymbol

    /** Is this a reach reference of the form `x*` or a readOnly or maybe variant
     *  of a reach reference?
     */
    final def isReach(using Context): Boolean = this ne stripReach

    final def stripMaybe(using Context): Capability = this match
      case Maybe(ref1) => ref1
      case _ => this

    final def stripReadOnly(using Context): Capability = this match
      case ReadOnly(ref1) => ref1
      case Maybe(ref1) => ref1.stripReadOnly.maybe
      case _ => this

    /** Drop restrictions with clss `cls` or a superclass of `cls` */
    final def stripRestricted(cls: ClassSymbol)(using Context): Capability = this match
      case Restricted(ref1, cls1) if cls.isSubClass(cls1) => ref1
      case ReadOnly(ref1) => ref1.stripRestricted(cls).readOnly
      case Maybe(ref1) => ref1.stripRestricted(cls).maybe
      case _ => this

    final def stripRestricted(using Context): Capability =
      stripRestricted(defn.NothingClass)

    final def stripReach(using Context): Capability = this match
      case Reach(ref1) => ref1
      case ReadOnly(ref1) => ref1.stripReach.readOnly
      case Restricted(ref1, cls) => ref1.stripReach.restrict(cls)
      case Maybe(ref1) => ref1.stripReach.maybe
      case _ => this

    /** Is this reference a root capability or a derived version of one?
     *  These capabilities have themselves as their captureSetOfInfo.
     */
    final def isTerminalCapability(using Context): Boolean =
      core.isInstanceOf[RootCapability]

    /** Is the reference tracked? This is true if it can be tracked and the capture
     *  set of the underlying type is not always empty. Also excluded are references
     *  that come from source files that were not capture checked and that have
     *  `Fluid` capture sets.
     */
    final def isTracked(using Context): Boolean = this.core match
      case _: RootCapability => true
      case tp: CoreCapability =>
        tp.isTrackableRef
        && {
          val cs = captureSetOfInfo
          !cs.isAlwaysEmpty && cs != CaptureSet.Fluid
        }

    /** An exclusive capability is a capability that derives
     *  indirectly from a maximal capability without going through
     *  a read-only capability or a capability classified as SharedCapability first.
     *  @param required  if true, exclusivity can be obtained by setting the mutability
     *                   status of some capture set variable from Ignored to Writer.
     */
    final def isExclusive(required: Boolean = false)(using Context): Boolean =
      !isReadOnly
      && !classifier.derivesFrom(defn.Caps_SharedCapability)
      && (isTerminalCapability || captureSetOfInfo.isExclusive(required))

    /** Similar to isExlusive, but also includes capabilties with capture
     *  set variables in their info whose status is still open.
     */
    final def maybeExclusive(using Context): Boolean =
      !isReadOnly && (isTerminalCapability || captureSetOfInfo.maybeExclusive)

    final def isWellformed(using Context): Boolean = this match
      case self: CoreCapability => self.isTrackableRef
      case _ => true

    /** Under separation checking: Is this a mutable var owned by a term that is
     *  not annotated with @untrackedCaptures? Such mutable variables need to be
     *  tracked as capabilities. Since mutable variables are not trackable, we do
     *  this by adding a varMirror symbol to such variables which represents the capability.
     */
    final def isLocalMutable(using Context): Boolean = this match
      case tp @ TermRef(NoPrefix, _) =>
        ccConfig.strictMutability
        && tp.symbol.isMutableVar
        && !tp.symbol.hasAnnotation(defn.UntrackedCapturesAnnot)
      case _ => false

    /** The non-derived capability underlying this capability */
    final def core: CoreCapability | RootCapability = this match
      case self: (CoreCapability | RootCapability) => self
      case self: DerivedCapability => self.underlying.core

    /** The type underlying this capability, NoType for root capabilities */
    final def coreType: CoreCapability | NoType.type = core match
      case c: CoreCapability => c
      case _ => NoType

    /** The first element of this path type, skipping selections
     *  and qualifiers. Note that class parameter references are of
     *  the form this.C but their pathroot is still this.C, not this.
     */
    final def pathRoot(using Context): Capability = this match
      case LocalCap(pre: Capability) => pre.pathRoot
      case _: RootCapability => this
      case self: DerivedCapability => self.underlying.pathRoot
      case self: CoreCapability => self.dealias match
        case tp1: (TermRef | TypeRef) => // can't use NamedType here since it is not a capability
          if tp1.symbol.maybeOwner.isClass && !tp1.symbol.is(TypeParam) then
            tp1.prefix match
              case pre: Capability => pre.pathRoot
              case _ => tp1
          else tp1
        case tp1: CoreCapability => tp1
        case _ => self

    /** The logical owner of the root of this class:
    *   - If this path starts with `C.this`, the class `C`.
    *   - If it starts with a reference `r`, `r`'s owner.
    *   - If it starts with caps.any, the `scala.caps` package class.
    *   - If it starts with a LocalCap instance, its owner.
    *   - If it starts with a ParamRef or a ResultCap, NoSymbol.
    */
    final def pathOwner(using Context): Symbol = pathRoot match
      case tp1: ThisType => tp1.cls
      case tp1: NamedType => tp1.symbol.owner
      case _: GlobalCap => defn.CapsModule.moduleClass
      case tp1: LocalCap => tp1.ccOwner
      case _ => NoSymbol

    final def paramPathRoot(using Context): Type = core match
      case tp1: NamedType =>
        tp1.prefix match
          case _: ThisType | NoPrefix =>
            if tp1.symbol.is(Param) || tp1.symbol.is(ParamAccessor) then tp1
            else NoType
          case prefix: CoreCapability => prefix.paramPathRoot
          case _ => NoType
      case tp1: ParamRef => tp1
      case _ => NoType

    final def isParamPath(using Context): Boolean = paramPathRoot.exists

    /** Compute ccOwner or (part of level owner).
     *  @param mapUnscoped  if true, return the enclosing toplevel class for LocalCaps
     *                      classified as Unscoped that don't have a prefix
     */
    private def computeOwner(mapUnscoped: Boolean)(using Context): Symbol = this match
      case self: ThisType => self.cls
      case TermRef(prefix: Capability, _) => prefix.computeOwner(mapUnscoped)
      case self: NamedType => self.symbol
      case self: DerivedCapability => self.underlying.computeOwner(mapUnscoped)
      case self: LocalCap =>
        val setOwner = self.hiddenSet.owner
        self.prefix match
          case prefix: ThisType if setOwner.isTerm && setOwner.owner == prefix.cls =>
            setOwner
          case prefix: Capability => prefix.computeOwner(mapUnscoped)
          case NoPrefix if mapUnscoped && classifier.derivesFrom(defn.Caps_Unscoped) =>
            ctx.owner.topLevelClass
              .orElse: // fallback needed if ctx.owner is a toplevel module val
                assert(ctx.owner.is(ModuleVal))
                ctx.owner
          case _ => setOwner
      case _ /* : GlobalCap | ResultCap | ParamRef */ => NoSymbol

    final def ccOwner(using Context): Symbol = computeOwner(mapUnscoped = false)

    final def visibility(using Context): Symbol = this match
      case self: LocalCap => adjustOwner(computeOwner(mapUnscoped = true))
      case _ =>
        val vis = computeOwner(mapUnscoped = true)
        if vis.is(Param) then vis.owner else vis

    /** The symbol that represents the level closest-enclosing ccOwner.
     *  Symbols representing levels are
     *   - class symbols, but not inner (non-static) module classes
     *   - method symbols, but not accessors or constructors
     *  For Unscoped LocalCaps the level owner is the top-level class.
     */
    final def levelOwner(using Context): Symbol =
      adjustOwner(computeOwner(mapUnscoped = true))

    final def adjustOwner(owner: Symbol)(using Context): Symbol =
      if !owner.exists
        || owner.isClass && (!owner.is(Flags.Module) || owner.isStatic)
        || owner.is(Flags.Method, butNot = Flags.Accessor)
      then owner
      else adjustOwner(owner.owner)

    /** Tests whether the capability derives from capability class `cls`. */
    def derivesFromCapTrait(cls: ClassSymbol)(using Context): Boolean = this match
      case Reach(ref1) => ref1.widen.derivesFromCapTraitDeeply(cls)
      case self: DerivedCapability => self.underlying.derivesFromCapTrait(cls)
      case self: CoreCapability => self.superType.derivesFromCapTrait(cls)
      case _ => false

    def derivesFromCapability(using Context): Boolean = derivesFromCapTrait(defn.Caps_Capability)
    def derivesFromStateful(using Context): Boolean = derivesFromCapTrait(defn.Caps_Stateful)
    def derivesFromShared(using Context): Boolean = derivesFromCapTrait(defn.Caps_SharedCapability)
    def derivesFromUnscoped(using Context): Boolean = derivesFromCapTrait(defn.Caps_Unscoped)

    /** The capture set consisting of exactly this reference */
    def singletonCaptureSet(using Context): CaptureSet.Const =
      if mySingletonCaptureSet == null then
        mySingletonCaptureSet = CaptureSet(this)
      mySingletonCaptureSet.uncheckedNN

    /** The capture set of the type underlying this reference */
    def captureSetOfInfo(using Context): CaptureSet =
      if captureSetValid == currentId then myCaptureSet.nn
      else if myCaptureSet.asInstanceOf[AnyRef] eq CaptureSet.Pending then CaptureSet.empty
      else
        myCaptureSet = CaptureSet.Pending
        val computed = CaptureSet.ofInfo(this)
        def isProvisional = this.core match
          case core: TypeProxy => !core.underlying.exists || core.underlying.isProvisional
          case _ => false
        if !ccConfig.cacheCaptureSetOfInfo
            || !isCaptureChecking
            || ctx.mode.is(Mode.IgnoreCaptures)
            || isProvisional
        then
          myCaptureSet = null
        else
          myCaptureSet = computed
          captureSetValid = currentId
        computed

    /** The elements hidden by this capability, if this is a LocalCap
     *  or a derived version of one. Read-only status and restrictions
     *  are transferred from the capability to its hidden set.
     */
    def hiddenSet(using Context): Refs = computeHiddenSet(identity)

    /** Compute result based on hidden set of this capability.
     *  Restrictions and read-only status transfer from the capability to its
     *  hidden set.
     *  @param  f   a function that gets applied to all detected hidden sets
     */
    def computeHiddenSet(f: Refs => Refs)(using Context): Refs = this match
      case self: LocalCap => f(self.hiddenSet.elems)
      case Restricted(elem1, cls) => elem1.computeHiddenSet(f).map(_.restrict(cls))
      case ReadOnly(elem1) => elem1.computeHiddenSet(f).map(_.readOnly)
      case _ => emptyRefs

    /** The transitive classifiers of this capability. */
    def transClassifiers(using Context): Classifiers =
      def toClassifiers(cls: ClassSymbol): Classifiers =
        if cls == defn.AnyClass then Unclassified
        else ClassifiedAs(cls :: Nil)
      if classifiersValid != currentId then
        myClassifiers = this match
          case self: LocalCap =>
            toClassifiers(self.hiddenSet.classifier)
          case self: RootCapability =>
            Unclassified
          case Restricted(_, cls) =>
            assert(cls != defn.AnyClass)
            if cls == defn.NothingClass then ClassifiedAs(Nil)
            else ClassifiedAs(cls :: Nil)
          case ReadOnly(ref1) =>
            ref1.transClassifiers
          case Maybe(ref1) =>
            ref1.transClassifiers
          case Reach(_) =>
            captureSetOfInfo.transClassifiers
          case self: CoreCapability =>
            if self.derivesFromCapability then toClassifiers(self.inheritedClassifier)
            else captureSetOfInfo.transClassifiers
        if myClassifiers != UnknownClassifier then
          classifiersValid == currentId
      myClassifiers
    end transClassifiers

    def tryClassifyAs(cls: ClassSymbol)(using Context): Boolean =
      cls == defn.AnyClass
      || this.match
        case self: LocalCap =>
          if self.isClassified then self.hiddenSet.classifier.derivesFrom(cls)
          else self.hiddenSet.tryClassifyAs(cls)
        case self: RootCapability =>
          true
        case Restricted(_, cls1) =>
          assert(cls != defn.AnyClass)
          cls1.isSubClass(cls)
        case ReadOnly(ref1) =>
          ref1.tryClassifyAs(cls)
        case Maybe(ref1) =>
          ref1.tryClassifyAs(cls)
        case Reach(_) =>
          captureSetOfInfo.tryClassifyAs(cls)
        case self: CoreCapability =>
          if self.derivesFromCapability then self.derivesFrom(cls)
          else captureSetOfInfo.tryClassifyAs(cls)

    def isKnownClassifiedAs(cls: ClassSymbol)(using Context): Boolean =
      transClassifiers match
        case ClassifiedAs(cs) => cs.forall(_.isSubClass(cls))
        case _ => false

    def isKnownEmpty(using Context): Boolean = this match
      case Restricted(ref1, cls) =>
        val isEmpty = ref1.transClassifiers match
          case ClassifiedAs(cs) =>
            cs.forall(c => leastClassifier(c, cls) == defn.NothingClass)
          case _ => false
        isEmpty || ref1.isKnownEmpty
      case ReadOnly(ref1) => ref1.isKnownEmpty
      case Maybe(ref1) => ref1.isKnownEmpty
      case _: RootCapability => false
      case _: ObjectCapability if ccState.isSepCheck =>
        captureSetOfInfo.dropEmpties().elems.isEmpty
      case _ => false

    def invalidateCaches() =
      captureSetValid = invalid
      classifiersValid = invalid

    /**  x subsumes x
     *   x =:= y       ==>  x subsumes y
     *   x subsumes y  ==>  x subsumes y.f
     *   x subsumes y  ==>  x* subsumes y, x subsumes y?
     *   x subsumes y  ==>  x* subsumes y*, x? subsumes y?
     *   x: x1.type /\ x1 subsumes y  ==>  x subsumes y
     *   X = CapSet^cx, exists rx in cx, rx subsumes y     ==>  X subsumes y
     *   Y = CapSet^cy, forall ry in cy, x subsumes ry     ==>  x subsumes Y
     *   X: CapSet^c1...CapSet^c2, (CapSet^c1) subsumes y  ==>  X subsumes y
     *   Y: CapSet^c1...CapSet^c2, x subsumes (CapSet^c2)  ==>  x subsumes Y
     *   Contains[X, y]  ==>  X subsumes y
     */
    final def subsumes(y: Capability)(using ctx: Context)(using vs: VarState = VarState.Separate): Boolean =

      /** Are `x` and `y` capabilities such that x subsumes y? */
      def subsumingRefs(x: Type | Capability, y: Type | Capability): Boolean = x match
        case x: Capability => y match
          case y: Capability => x.subsumes(y)
          case _ => false
        case _ => false

      /** Perform `test` on all object capabilities in `info` */
      def viaInfo(info: Type)(test: Type => Boolean): Boolean = info.dealias match
        case info: ObjectCapability => test(info)
        case CapturingType(parent, _) => viaInfo(parent)(test)
        case info: AndType => viaInfo(info.tp1)(test) || viaInfo(info.tp2)(test)
        case info: OrType => viaInfo(info.tp1)(test) && viaInfo(info.tp2)(test)
        case _ => false

      try (this eq y)
      || maxSubsumes(y, canAddHidden = !vs.isOpen)
          // if vs is open, we should add new elements to the set containing `this`
          // instead of adding them to the hidden set of of `this`.
      || y.match
        case y: TermRef =>
            y.prefix.match
              case ypre: Capability =>
                this.subsumes(ypre)
                || this.match
                    case x @ TermRef(xpre: Capability, _) if x.symbol == y.symbol =>
                      // To show `{x.f} <:< {y.f}`, it is important to prove `x` and `y`
                      // are equvalent, which means `x =:= y` in terms of subtyping,
                      // not just `{x} =:= {y}` in terms of subcapturing.
                      // It is possible to construct two singleton types `x` and `y`,
                      // which subsume each other, but are not equal references.
                      // See `tests/neg-custom-args/captures/path-prefix.scala` for example.
                      withMode(Mode.IgnoreCaptures):
                        TypeComparer.isSameRef(xpre, ypre)
                    case _ =>
                      false
              case _ => false
          || viaInfo(y.info)(subsumingRefs(this, _))
        case Maybe(y1) => this.stripMaybe.subsumes(y1)
        case ReadOnly(y1) => this.stripReadOnly.subsumes(y1)
        case Restricted(y1, cls) => this.stripRestricted(cls).subsumes(y1)
        case y: TypeRef if y.derivesFrom(defn.Caps_CapSet) =>
          // The upper and lower bounds don't have to be in the form of `CapSet^{...}`.
          // They can be other capture set variables, which are bounded by `CapSet`,
          // like `def test[X^, Y^, Z >: X <: Y]`.
          y.info match
            case TypeBounds(_, hi @ CapturingType(parent, refs)) =>
              refs.elems.forall(this.subsumes)
            case TypeBounds(_, hi: Capability) =>
              this.subsumes(hi)
            case _ =>
              y.captureSetOfInfo.elems.forall(this.subsumes)
        case _ => false
      || this.match
          case Reach(x1) => x1.subsumes(y.stripReach)
          case Restricted(x1, cls) => y.isKnownClassifiedAs(cls) && x1.subsumes(y)
          case x: TermRef => viaInfo(x.info)(subsumingRefs(_, y))
          case x: TypeRef if assumedContainsOf(x).contains(y) => true
          case x: TypeRef if x.derivesFrom(defn.Caps_CapSet) =>
            x.info match
              case TypeBounds(CapturingType(_, lorefs), _) =>
                lorefs.elems.exists(_.subsumes(y))
              case TypeBounds(lo: Capability, _) =>
                lo.subsumes(y)
              case _ =>
                x.captureSetOfInfo.elems.exists(_.subsumes(y))
          case _ => false
      catch case ex: AssertionError =>
        println(i"error while subsumes $this >> $y")
        throw ex
    end subsumes

    /** This is a maximal capability that subsumes `y` in given context and VarState.
     *  @param canAddHidden  If true we allow maximal capabilities to subsume all other capabilities.
     *                       We add those capabilities to the hidden set if this is a LocalCap instance.
     *                       If false we only accept `y` elements that are already in the
     *                       hidden set of this LocalCap instance. The idea is that in a VarState that
     *                       accepts additions we first run `maxSubsumes` with `canAddHidden = false`
     *                       so that new variables get added to the sets. If that fails, we run
     *                       the test again with canAddHidden = true as a last effort before we
     *                       fail a comparison.
     */
    def maxSubsumes(y: Capability, canAddHidden: Boolean)(using ctx: Context)(using vs: VarState = VarState.Separate): Boolean =
      (this eq y)
      || this.match
        case x: LocalCap =>
          def classifierOK =
            if y.tryClassifyAs(x.hiddenSet.classifier) then true
            else
              capt.println(i"$y cannot be classified as $x")
              false

          def prefixAllowsAddHidden: Boolean =
            CCState.collapseLocalCaps || x.prefix.match
              case NoPrefix => true
              case pre: ThisType => x.ccOwner.isContainedIn(pre.cls)
              case pre =>
                capt.println(i"LocalCap not open $x, ${x.rootId}, $pre, ${x.ccOwner.skipStrictValDef.thisType}")
                false

          vs.ifNotSeen(this)(x.hiddenSet.elems.exists(_.subsumes(y)))
          || x.coversLocalCap(y)
          || x.acceptsLevelOf(y)
              && classifierOK
              && canAddHidden
              && prefixAllowsAddHidden
              && vs.addHidden(x.hiddenSet, y)
        case x: ResultCap =>
          y match
            case y: ResultCap => vs.unify(x, y)
            case _ => y.derivesFromShared
        case _: GlobalCap =>
          y match
            case _: GlobalCap => this eq y
            case _: ResultCap => false
            case _: LocalCap if CCState.collapseLocalCaps => true
            case _ =>
              y.derivesFromShared
              || canAddHidden && vs != VarState.HardSeparate && CCState.globalCapIsRoot
        case Restricted(x1, cls) =>
          y.isKnownClassifiedAs(cls) && x1.maxSubsumes(y, canAddHidden)
        case _ =>
          y match
            case ReadOnly(y1) => this.stripReadOnly.maxSubsumes(y1, canAddHidden)
            case Restricted(y1, cls) => this.stripRestricted(cls).maxSubsumes(y1, canAddHidden)
            case _ => false

    /** `x covers y` if we should retain `y` when computing the overlap of
     *  two footprints which have `x` respectively `y` as elements.
     *  We assume that .rd have already been stripped on both sides.
     *  We have:
     *
     *   x covers x
     *   x covers y  ==>  x covers y.f
     *   x covers y  ==>  x* covers y*, x? covers y?
     *   x covers y  ==>  <any hiding x> covers y
     *   x covers y  ==>  x.only[C] covers y, x covers y.only[C]
     *
     *   TODO what other clauses from subsumes do we need to port here?
     *   The last clause is a conservative over-approximation: basically, we can't achieve
     *   separation by having different classifiers for now. It would be good to
     *   have a test that would expect such separation, then we can try to refine
     *   the clause to make the test pass.
     */
    final def covers(y: Capability)(using Context): Boolean =
      val seen: util.EqHashSet[LocalCap] = new util.EqHashSet

      def recur(x: Capability, y: Capability): Boolean =
        (x eq y)
        || y.match
            case y @ TermRef(ypre: Capability, _) =>
              recur(x, ypre)
            case Reach(y1) =>
              x match
                case Reach(x1) => recur(x1, y1)
                case _ => false
            case Maybe(y1) =>
              x match
                case Maybe(x1) => recur(x1, y1)
                case _ => false
            case Restricted(y1, _) =>
              recur(x, y1)
            case _ =>
              false
        || x.match
            case x: LocalCap =>
              if x.coversLocalCap(y) then true
              else if !seen.contains(x) then
                seen.add(x)
                x.hiddenSet.exists(recur(_, y))
              else false
            case Restricted(x1, _) => recur(x1, y)
            case _ => false

      recur(this, y)
    end covers

    /** `x eq y` or `x` is a LocalCap, `y` is a LocalCap with prefix
     *  `p`, and there is a prefix of `p` that contains `x` in its
     *  capture set.
     */
    final def coversLocalCap(y: Capability)(using Context): Boolean =
      (this eq y) || this.match
        case x: LocalCap => y match
          case y: LocalCap =>
            x.origin match
              case Origin.InDecl(sym, _) =>
                def occursInPrefix(pre: Type): Boolean = pre match
                  case pre @ TermRef(pre1, _) =>
                    pre.symbol == sym
                    && pre.info.captureSet.elems.contains(x)
                    || occursInPrefix(pre1)
                  case _ => false
                occursInPrefix(y.prefix)
              case _ => false
          case _ => false
        case _ => false

    def assumedContainsOf(x: TypeRef)(using Context): SimpleIdentitySet[Capability] =
      CaptureSet.assumedContains.getOrElse(x, SimpleIdentitySet.empty)

    /** The type representing this capability.
     *  Note this method does not distinguish different `RootCapability` instances,
     *  and should only be used for printing or phases not related to CC.
     */
    def toType(using Context): Type = this match
      case c: RootCapability => defn.Caps_any.termRef
      case c: CoreCapability => c
      case c: DerivedCapability =>
        val c1 = c.underlying.toType
        c match
          case _: ReadOnly => ReadOnlyCapability(c1)
          case Restricted(_, cls) => OnlyCapability(c1, cls)
          case _: Reach => ReachCapability(c1)
          case _: Maybe => MaybeCapability(c1)
          case _ => c1

    def showAsCapability(using Context) =
      i"${ctx.printer.toTextCapability(this).show}"

    def toText(printer: Printer): Text = printer.toTextCapability(this)
  end Capability

  /** Result type of `transClassifiers`. Interprete as follows:
   *    UnknownClassifier: No list could be computed since some capture sets
   *                       are still unsolved variables
   *    Unclassified     : No set exists since some parts of tcs are not classified
   *    ClassifiedAs(clss: All parts of tcss are classified with classes in clss
   */
  enum Classifiers derives CanEqual:
    case UnknownClassifier
    case Unclassified
    case ClassifiedAs(clss: List[ClassSymbol])

  export Classifiers.{UnknownClassifier, Unclassified, ClassifiedAs}

  /** The least classifier between `cls1` and `cls2`, which are either
   *  AnyClass, NothingClass, or a class directly extending caps.Classifier.
   *  @return if one of cls1, cls2 is a subclass of the other, the subclass
   *          otherwise NothingClass (which is a subclass of all classes)
   */
  def leastClassifier(cls1: ClassSymbol, cls2: ClassSymbol)(using Context): ClassSymbol =
    if cls1.isSubClass(cls2) then cls1
    else if cls2.isSubClass(cls1) then cls2
    else defn.NothingClass

  /** The smallest list D of class symbols in cs1 and cs2 such that
   *  every class symbol in cs1 and cs2 is a subclass of a class symbol in D
   */
  def dominators(cs1: List[ClassSymbol], cs2: List[ClassSymbol])(using Context): List[ClassSymbol] =
    // Drop classes that subclass classes of the other set
    // @param proper  If true, only drop proper subclasses of a class of the other set
    def filterSub(cs1: List[ClassSymbol], cs2: List[ClassSymbol], proper: Boolean) =
      cs1.filter: cls1 =>
        !cs2.exists: cls2 =>
          cls1.isSubClass(cls2) && (!proper || cls1 != cls2)
    filterSub(cs1, cs2, proper = true) ++ filterSub(cs2, cs1, proper = false)

  def joinClassifiers(cs1: Classifiers, cs2: Classifiers)(using Context): Classifiers =
    (cs1, cs2) match
      case (Unclassified, _) | (_, Unclassified) =>
        Unclassified
      case (UnknownClassifier, _) | (_, UnknownClassifier) =>
        UnknownClassifier
      case (ClassifiedAs(cs1), ClassifiedAs(cs2)) =>
        ClassifiedAs(dominators(cs1, cs2))

  /** The place of - and cause for - creating a LocalCap capability. Used for
   *  error diagnostics
   */
  enum Origin derives CanEqual:
    case InDecl(sym: Symbol, fields: List[Symbol] = Nil)
    case TypeArg(tp: Type)
    case UnsafeAssumePure
    case Formal(pref: ParamRef, app: tpd.Apply)
    case ResultInstance(methType: Type, meth: Symbol)
    case UnapplyInstance(info: MethodType)
    case LocalInstance(restpe: Type)
    case NewInstance(tp: Type, fields: List[Symbol])
    case LambdaExpected(respt: Type)
    case LambdaActual(restp: Type)
    case OverriddenType(member: Symbol)
    case DeepCS(ref: TypeRef)
    case Parameter(param: Symbol)
    case Unknown

    def contributingFields: List[Symbol] = this match
      case InDecl(sym, fields) => fields
      case NewInstance(tp, fields) => fields
      case _ => Nil

    private def contributingStr(using Context): String =
      contributingFields match
        case Nil => ""
        case field :: Nil => s" with contributing field $field"
        case fields => s" with contributing fields ${fields.map(_.show).mkString(", ")}"

    def explanation(using Context): String = this match
      case InDecl(sym, fields) =>
        if sym.is(Method) then i" in the result type of $sym$contributingStr"
        else if sym.exists then i" in the type of $sym$contributingStr"
        else ""
      case TypeArg(tp: Type) =>
        i" of type argument $tp"
      case UnsafeAssumePure =>
        " when instantiating argument of unsafeAssumePure"
      case Formal(pref, app) =>
        val meth = app.symbol
        if meth.exists
        then i" when checking argument to parameter ${pref.paramName} of $meth"
        else ""
      case ResultInstance(mt, meth) =>
        val methDescr = if meth.exists then i"$meth's type " else ""
        i" when instantiating $methDescr$mt"
      case UnapplyInstance(info) =>
        i" when instantiating argument of unapply with type $info"
      case LocalInstance(restpe) =>
        i" when instantiating expected result type $restpe of function literal"
      case NewInstance(tp, fields) =>
        i" when constructing instance $tp$contributingStr"
      case LambdaExpected(respt) =>
        i" when instantiating expected result type $respt of lambda"
      case LambdaActual(restp: Type) =>
        i" when instantiating result type $restp of lambda"
      case OverriddenType(member: Symbol) =>
        i" when instantiating upper bound of member overridden by $member"
      case DeepCS(ref: TypeRef) =>
        i" when computing deep capture set of $ref"
      case Parameter(param) =>
        i" of parameter $param of ${param.owner}"
      case Unknown =>
        ""
  end Origin

  // ---------- Maps between different kinds of root capabilities -----------------


  /** Map each occurrence of `caps.any` to a different LocalCap instance
   *  Exception: CapSet^ stays as it is.
   */
  class GlobalCapToLocal(origin: Origin)(using Context) extends BiTypeMap, FollowAliasesMap:
    thisMap =>

    override def apply(t: Type) =
      if variance < 0 then t
      else t match
        case t @ CapturingType(_, _) =>
          mapOver(t)
        case t @ AnnotatedType(parent, ann: RetainingAnnotation)
        if ann.isStrict && ann.toCaptureSet.elems.exists(_.core.isInstanceOf[GlobalCap]) =>
          // Applying `this` can cause infinite recursion in some cases during printing.
          // scalac -Xprint:all tests/pos/i23885/S_1.scala tests/pos/i23885/S_2.scala
          mapOver(CapturingType(this(parent), ann.toCaptureSet))
        case t @ AnnotatedType(parent, ann) =>
          t.derivedAnnotatedType(this(parent), ann)
        case t @ defn.RefinedFunctionOf(mt) =>
          t.derivedRefinedType(refinedInfo = mapOver(mt))
        case _ =>
          mapFollowingAliases(t)

    override def mapCapability(c: Capability, deep: Boolean): Capability = c match
      case GlobalAny => LocalCap(origin)
      case _ => super.mapCapability(c, deep)

    override def fuse(next: BiTypeMap)(using Context) = next match
      case next: Inverse => assert(false); Some(IdentityTypeMap)
      case _ => None

    override def toString = "GlobalToLocalCap"

    class Inverse extends BiTypeMap, FollowAliasesMap:
      def apply(t: Type): Type = t match
        case t @ CapturingType(_, refs) => mapOver(t)
        case _ => mapFollowingAliases(t)

      override def mapCapability(c: Capability, deep: Boolean): Capability = c match
        case _: LocalCap => GlobalAny
        case _ => super.mapCapability(c, deep)

      def inverse = thisMap
      override def toString = thisMap.toString + ".inverse"

    lazy val inverse = Inverse()

  end GlobalCapToLocal

  /** Maps caps.any to LocalCap instances. GlobalToLocalCap is a BiTypeMap since we don't want to
   *  freeze a set when it is mapped. On the other hand, we do not want LocalCap
   *  values to flow back to caps.any since that would fail disallowRootCapability
   *  tests elsewhere. We therefore use `withNoVarsMapped` to prevent
   *  the map being installed for future use.
   */
  def globalCapToLocal(tp: Type, origin: Origin)(using Context): Type =
    ccState.withNoVarsMapped:
      GlobalCapToLocal(origin)(tp)

  /** Maps all LocalCap instances to caps.any */
  def localCapToGlobal(param: Symbol, tp: Type)(using Context): Type =
    GlobalCapToLocal(Origin.Parameter(param)).inverse(tp)

  /** The local dual of a result type of a closure type.
   *  @param binder  the method type of the anonymous function whose result is mapped
   *  @pre           the context's owner is the anonymous function
   */
  class Internalize(binder: MethodType)(using Context) extends BiTypeMap:
    thisMap =>

    val sym = ctx.owner
    assert(sym.isAnonymousFunction)
    val paramSyms = atPhase(ctx.phase.prev):
      // We need to ask one phase before since `sym` should not be completed as a side effect.
      // The result of Internalize is used to se the result type of an anonymous function, and
      // the new info of that function is built with the result.
      sym.paramSymss.head
    val resultToAny = EqHashMap[ResultCap, LocalCap]()
    val anyToResult = EqHashMap[LocalCap, ResultCap]()

    override def apply(t: Type) =
      if variance < 0 then t
      else t match
        case t: ParamRef =>
          if t.binder == this.binder then paramSyms(t.paramNum).termRef else t
        case _ => mapOver(t)

    override def mapCapability(c: Capability, deep: Boolean): Capability = c match
      case r: ResultCap if r.binder == this.binder =>
        resultToAny.get(r) match
          case Some(f) => f
          case None =>
            val f = LocalCap(Origin.LocalInstance(binder.resType))
            resultToAny(r) = f
            anyToResult(f) = r
            f
      case _ =>
        super.mapCapability(c, deep)

    class Inverse extends BiTypeMap:
      def apply(t: Type): Type =
        if variance < 0 then t
        else t match
          case t: TermRef if paramSyms.contains(t) =>
            binder.paramRefs(paramSyms.indexOf(t.symbol))
          case _ => mapOver(t)

      override def mapCapability(c: Capability, deep: Boolean): Capability = c match
        case f: LocalCap if f.owner == sym =>
          anyToResult.get(f) match
            case Some(r) => r
            case None =>
              val r = ResultCap(binder)
              resultToAny(r) = f
              anyToResult(f) = r
              r
        case _ => super.mapCapability(c, deep)

      def inverse = thisMap
      override def toString = thisMap.toString + ".inverse"
    end Inverse

    override def toString = "InternalizeClosureResult"
    def inverse = Inverse()
  end Internalize

  /** Map top-level free ResultCaps one-to-one to LocalCap instances */
  def resultToAny(tp: Type, origin: Origin)(using Context): Type =
    val subst = new TypeMap:
      val seen = EqHashMap[ResultCap, LocalCap | GlobalCap]()
      var localBinders: SimpleIdentitySet[MethodType] = SimpleIdentitySet.empty

      def apply(t: Type): Type = t match
        case t: MethodType =>
          // skip parameters
          val saved = localBinders
          if t.marksExistentialScope then localBinders = localBinders + t
          try t.derivedLambdaType(resType = this(t.resType))
          finally localBinders = saved
        case t: PolyType =>
          // skip parameters
          t.derivedLambdaType(resType = this(t.resType))
        case _ =>
          mapOver(t)

      override def mapCapability(c: Capability, deep: Boolean) = c match
        case c @ ResultCap(binder) =>
          if localBinders.contains(binder) then c // keep bound references
          else
            // Create a LocalCap skolem that does not subsume anything
            def localCapSkolem =
              val c = LocalCap(origin)
              c.hiddenSet.markSolved(provisional = false)
              c
            seen.getOrElseUpdate(c, localCapSkolem) // map free references to LocalCap
        case _ => super.mapCapability(c, deep)
    end subst

    subst(tp)
  end resultToAny

  abstract class CapMap(using Context) extends BiTypeMap:
    override def mapOver(t: Type): Type = t match
      case t @ FunctionOrMethod(args, res) if variance > 0 && !t.isAliasFun =>
        t // `t` should be mapped in this case by a different call to `toResult`. See [[toResultInResults]].
      case t: (LazyRef | TypeVar) =>
        mapConserveSuper(t)
      case _ =>
        super.mapOver(t)

  class ToResult(localResType: Type, mt: MethodicType, sym: Symbol, fail: Message => Unit)(using Context) extends CapMap:

    def apply(t: Type) = mapOver(t)

    override def mapCapability(c: Capability, deep: Boolean) = c match
      case c: LocalCap =>
        if variance > 0 then
          if sym.isAnonymousFunction && c.classifier.derivesFrom(defn.Caps_Unscoped) then
            c
          else if sym.exists && !c.ccOwner.isContainedIn(sym) then
            //println(i"not mapping $c with ${c.ccOwner} in $sym")
            c
          else
            ResultCap(mt).setOrigin(c)
        else
          if variance == 0 then
            fail(em"""$localResType captures the root capability `any` in invariant position.
                     |This capability cannot be converted to a fresh capability in the result type of a function.""")
          // we accept variance < 0, and leave the `any` as it is          c
          c
      case GlobalFresh if variance > 0 =>
        ResultCap(mt) // if variance <= 0 we leave the fresh to be flagged later
      case _ =>
        super.mapCapability(c, deep)

      //.showing(i"mapcap $t = $result")
    override def toString = "toVar"

    object inverse extends BiTypeMap:
      def apply(t: Type) = mapOver(t)

      override def mapCapability(c: Capability, deep: Boolean) = c match
        case c @ ResultCap(`mt`) =>
          val primary = c.primaryResultCap
          primary.origin match
            case _: GlobalCap =>
              val localCap = LocalCap(Origin.LocalInstance(mt.resType))
              primary.setOrigin(localCap)
              localCap
            case origin: LocalCap =>
              origin
        case _ =>
          super.mapCapability(c, deep)

      def inverse = ToResult.this
      override def toString = "toVar.inverse"
    end inverse
  end ToResult

  /** Replace all occurrences of `caps.any` or LocalCap in parts of this type by an existentially bound
   *  variable bound by `mt`. Stop at function or method types since these have been mapped before.
   */
  def toResult(tp: Type, mt: MethodicType, sym: Symbol, fail: Message => Unit)(using Context): Type =
    ToResult(tp, mt, sym, fail)(tp)
end Capabilities
