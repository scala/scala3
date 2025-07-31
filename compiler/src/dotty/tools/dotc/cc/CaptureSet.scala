package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Flags.*, Contexts.*, Decorators.*
import config.Printers.{capt, captDebug}
import Annotations.Annotation
import annotation.threadUnsafe
import annotation.constructorOnly
import annotation.internal.sharable
import reporting.trace
import printing.{Showable, Printer}
import printing.Texts.*
import util.{SimpleIdentitySet, Property}
import typer.ErrorReporting.Addenda
import util.common.alwaysTrue
import scala.collection.{mutable, immutable}
import TypeComparer.ErrorNote
import CCState.*
import TypeOps.AvoidMap
import compiletime.uninitialized
import Capabilities.*

/** A class for capture sets. Capture sets can be constants or variables.
 *  Capture sets support inclusion constraints <:< where <:< is subcapturing.
 *
 *  They also allow
 *   - mapping with functions from elements to capture sets
 *   - filtering with predicates on elements
 *   - intersecting wo capture sets
 *
 *  That is, constraints can be of the forms
 *
 *    cs1 <:< cs2
 *    cs1 = ∪ {f(x) | x ∈ cs2}     where f is a function from capabilities to capture sets.
 *    cs1 = ∪ {x | x ∈ cs2, p(x)}  where p is a predicate on capabilities
 *    cs1 = cs2 ∩ cs2
 *
 *  We call the resulting constraint system "monadic set constraints".
 *  To support capture propagation across maps, mappings are supported only
 *  if the mapped function is either a bijection or if it is idempotent
 *  on capabilities (c.f. doc comment on `map` below).
 */
sealed abstract class CaptureSet extends Showable:
  import CaptureSet.*
  import Mutability.*

  /** The elements of this capture set. For capture variables,
   *  the elements known so far.
   */
  def elems: Refs

  protected var myMut : Mutability = Ignored

  /** The access kind of this CaptureSet. */
  def mutability(using Context): Mutability = myMut

  def mutability_=(x: Mutability): Unit =
    myMut = x

  /** Mark this capture set as belonging to a Mutable type. */
  def setMutable()(using Context): Unit

  /** Is this capture set constant (i.e. not an unsolved capture variable)?
   *  Solved capture variables count as constant.
   */
  def isConst(using Context): Boolean

  /** Is this capture set always empty? For unsolved capture veriables, returns
   *  always false.
   */
  def isAlwaysEmpty(using Context): Boolean

  /** Is this set provisionally solved, so that another cc run might unfreeze it? */
  def isProvisionallySolved(using Context): Boolean

  /** An optional level limit, or undefinedLevel if none exists. All elements of the set
   *  must be at levels equal or smaller than the level of the set, if it is defined.
   */
  def level: Level

  /** An optional owner, or NoSymbol if none exists. Used for diagnstics
   */
  def owner: Symbol

  /** Is this capture set definitely non-empty? */
  final def isNotEmpty: Boolean = !elems.isEmpty

  /** If this is a Var, its `id`, otherwise -1 */
  def maybeId: Int = -1

  /** Convert to Const. @pre: isConst */
  def asConst(using Context): Const = this match
    case c: Const => c
    case v: Var =>
      assert(v.isConst)
      Const(v.elems)

  /** Cast to variable. @pre: !isConst */
  def asVar(using Context): Var =
    assert(!isConst)
    asInstanceOf[Var]

  /** Convert to Const with current elements unconditionally */
  def toConst: Const = this match
    case c: Const => c
    case v: Var => Const(v.elems)

  /** Does this capture set contain the root reference `cap` as element? */
  final def isUniversal(using Context) =
    elems.contains(GlobalCap)

  /** Does this capture set contain a root reference `cap` or `cap.rd` as element? */
  final def containsTerminalCapability(using Context) =
    elems.exists(_.isTerminalCapability)

  /** Does this capture set contain a ResultCap element? */
  final def containsResultCapability(using Context) =
    elems.exists(_.core.isInstanceOf[ResultCap])

  /** Does this capture set contain a GlobalCap or FreshCap, and at the same time
   *  does not contain a ResultCap?
   */
  final def containsCapOrFresh(using Context) =
    !containsResultCapability
    && elems.exists: elem =>
      elem.core match
        case GlobalCap => true
        case _: FreshCap => true
        case _ => false

  final def containsCap(using Context) =
    elems.exists(_.core eq GlobalCap)

  final def isReadOnly(using Context): Boolean =
    elems.forall(_.isReadOnly)

  final def isAlwaysReadOnly(using Context): Boolean = isConst && isReadOnly

  final def isExclusive(using Context): Boolean =
    elems.exists(_.isExclusive)

  /** Similar to isExlusive, but also includes capture set variables
   *  with unknown status.
   */
  final def maybeExclusive(using Context): Boolean = reporting.trace(i"mabe exclusive $this"):
    if isConst then elems.exists(_.maybeExclusive)
    else mutability != ReadOnly

  final def keepAlways: Boolean = this.isInstanceOf[EmptyWithProvenance]

  def failWith(fail: TypeComparer.ErrorNote)(using Context): false =
    TypeComparer.addErrorNote(fail)
    false

  /** Try to include an element in this capture set.
   *  @param elem    The element to be added
   *  @param origin  The set that originated the request, or `empty` if the request came from outside.
   *
   *  If the set already accounts for the element, return OK.
   *  Otherwise, try to add a new element to the set. This is OK if
   *    - the set is a variable, and
   *    - the element is not at a deeper nesting level than the set, and
   *    - the element can also be added (in mapped/filtered form) to all
   *      dependent sets.
   *  If the `origin` is the same as the `source` of the set variable, the
   *  element might be filtered or mapped according to the class of the variable.
   *  Otherwise, the element might have to be back-propagated to the source
   *  of the variable.
   *
   *  If the element itself cannot be added to the set for some reason, and the
   *  element is not the root capability, try instead to include its underlying
   *  capture set.
   */
  protected def tryInclude(elem: Capability, origin: CaptureSet)(using Context, VarState): Boolean = reporting.trace(i"try include $elem in $this # ${maybeId}"):
    accountsFor(elem) || addNewElem(elem)

  /** Try to include all element in `refs` to this capture set. */
  protected final def tryInclude(newElems: Refs, origin: CaptureSet)(using Context, VarState): Boolean =
    newElems.forall(tryInclude(_, origin))

  protected def mutableToReader(origin: CaptureSet)(using Context): Boolean =
    if mutability == Mutable then toReader() else true

  /** Add an element to this capture set, assuming it is not already accounted for,
   *  and omitting any mapping or filtering.
   *
   *  If the element itself cannot be added to the set for some reason, and the
   *  element is not the root capability, try instead to include its underlying
   *  capture set.
   */
  protected final def addNewElem(elem: Capability)(using ctx: Context, vs: VarState): Boolean =
    addThisElem(elem)
    || !elem.isTerminalCapability
        && vs.isOpen
        && {
          val underlying = elem.captureSetOfInfo
          val res = tryInclude(underlying.elems, this)
          if res then underlying.addDependent(this)
          res
        }

  /** Add a specific element, assuming it is not already accounted for,
   *  and omitting any mapping or filtering, without possibility to backtrack
   *  to the underlying capture set.
   */
  protected def addThisElem(elem: Capability)(using Context, VarState): Boolean

  protected def toReader()(using Context): Boolean

  protected def addIfHiddenOrFail(elem: Capability)(using ctx: Context, vs: VarState): Boolean =
    elems.exists(_.maxSubsumes(elem, canAddHidden = true))
    || failWith(IncludeFailure(this, elem))

  /** If this is a variable, add `cs` as a dependent set */
  protected def addDependent(cs: CaptureSet)(using Context, VarState): Boolean

  /** If `cs` is a variable, add this capture set as one of its dependent sets */
  protected def addAsDependentTo(cs: CaptureSet)(using Context): this.type =
    cs.addDependent(this)(using ctx, VarState.Unrecorded)
    this

  /** {x} <:< this   where <:< is subcapturing, but treating all variables
   *                 as frozen.
   */
  def accountsFor(x: Capability)(using ctx: Context)(using vs: VarState = VarState.Separate): Boolean =

    def debugInfo(using Context) =
      val suffix = if ctx.settings.YccVerbose.value then i" with ${x.captureSetOfInfo}" else ""
      i"$this accountsFor $x$suffix"

    def test(using Context) = reporting.trace(debugInfo):
      TypeComparer.noNotes: // Any failures in accountsFor should not lead to error notes
        elems.exists(_.subsumes(x))
        || // Even though subsumes already follows captureSetOfInfo, this is not enough.
           // For instance x: C^{y, z}. Then neither y nor z subsumes x but {y, z} accounts for x.
          !x.isTerminalCapability
          && !x.coreType.derivesFrom(defn.Caps_CapSet)
          && !(vs.isSeparating && x.captureSetOfInfo.containsTerminalCapability)
            // in VarState.Separate, don't try to widen to cap since that might succeed with {cap} <: {cap}
          && x.captureSetOfInfo.subCaptures(this, VarState.Separate)

    comparer match
      case comparer: ExplainingTypeComparer => comparer.traceIndented(debugInfo)(test)
      case _ => test
  end accountsFor

  /** A more optimistic version of accountsFor, which does not take variable supersets
   *  of the `x` reference into account. A set might account for `x` if it accounts
   *  for `x` in a state where we assume all supersets of `x` have just the elements
   *  known at this point. On the other hand if x's capture set has no known elements,
   *  a set `cs` might account for `x` only if it subsumes `x` or it contains the
   *  root capability `cap`.
   */
  def mightAccountFor(x: Capability)(using Context): Boolean =
    reporting.trace(i"$this mightAccountFor $x, ${x.captureSetOfInfo}?", show = true):
      CCState.withCollapsedFresh: // OK here since we opportunistically choose an alternative which gets checked later
        TypeComparer.noNotes:
          elems.exists(_.subsumes(x)(using ctx)(using VarState.ClosedUnrecorded))
      || !x.isTerminalCapability
        && {
          val elems = x.captureSetOfInfo.elems
          !elems.isEmpty && elems.forall(mightAccountFor)
        }

  /** A more optimistic version of subCaptures used to choose one of two typing rules
   *  for selections and applications. `cs1 mightSubcapture cs2` if `cs2` might account for
   *  every element currently known to be in `cs1`, and the same is not true in reverse
   *  when we compare elements of cs2 vs cs1.
   */
  def mightSubcapture(that: CaptureSet)(using Context): Boolean =
    elems.forall(that.mightAccountFor)
    && !that.elems.forall(this.mightAccountFor)

  /** The subcapturing test, taking an explicit VarState. */
  final def subCaptures(that: CaptureSet, vs: VarState)(using Context): Boolean =
    subCaptures(that)(using ctx, vs)

  /** The subcapturing test, using a given VarState */
  final def subCaptures(that: CaptureSet)(using ctx: Context, vs: VarState = VarState()): Boolean =
    TypeComparer.inNestedLevel:
      val this1 = this.adaptMutability(that)
      if this1 == null then false
      else if this1 ne this then
        capt.println(i"WIDEN ro $this with ${this.mutability} <:< $that with ${that.mutability} to $this1")
        this1.subCaptures(that, vs)
      else
        that.tryInclude(elems, this) && addDependent(that)

  /** Two capture sets are considered =:= equal if they mutually subcapture each other
   *  in a frozen state.
   */
  def =:= (that: CaptureSet)(using Context): Boolean =
       this.subCaptures(that, VarState.Separate)
    && that.subCaptures(this, VarState.Separate)

  def adaptMutability(that: CaptureSet)(using Context): CaptureSet | Null =
    val m1 = this.mutability
    val m2 = that.mutability
    if m1 == Mutable && m2 == Reader then this.readOnly
    else if m1 == Reader && m2 == Mutable then
      if that.toReader() then this else null
    else this

  /** The smallest capture set (via <:<) that is a superset of both
   *  `this` and `that`
   */
  def ++ (that: CaptureSet)(using Context): CaptureSet =
    if this.subCaptures(that, VarState.HardSeparate) then
      if that.isAlwaysEmpty && this.keepAlways then this else that
    else if that.subCaptures(this, VarState.HardSeparate) then this
    else if this.isConst && that.isConst then Const(this.elems ++ that.elems)
    else Union(this, that)

  def ++ (that: CaptureSet.Const)(using Context): CaptureSet.Const =
    Const(this.elems ++ that.elems)

  /** The smallest superset (via <:<) of this capture set that also contains `ref`.
   */
  def + (ref: Capability)(using Context): CaptureSet =
    this ++ ref.singletonCaptureSet

  /** The largest capture set (via <:<) that is a subset of both `this` and `that`
   */
  def **(that: CaptureSet)(using Context): CaptureSet =
    if this.subCaptures(that, VarState.Closed()) then this
    else if that.subCaptures(this, VarState.Closed()) then that
    else if this.isConst && that.isConst then Const(elemIntersection(this, that))
    else Intersection(this, that)

  /** The largest subset (via <:<) of this capture set that does not account for
   *  any of the elements in the constant capture set `that`
   */
  def -- (that: CaptureSet.Const)(using Context): CaptureSet =
    if this.isConst then
      val elems1 = elems.filter(!that.accountsFor(_))
      if elems1.size == elems.size then this else Const(elems1)
    else
      if that.isAlwaysEmpty then this else Diff(asVar, that)

  /** The largest subset (via <:<) of this capture set that does not account for `ref` */
  def - (ref: Capability)(using Context): CaptureSet =
    this -- ref.singletonCaptureSet

  /** The largest subset (via <:<) of this capture set that only contains elements
   *  for which `p` is true.
   */
  def filter(p: Context ?=> Capability => Boolean)(using Context): CaptureSet =
    if this.isConst then
      val elems1 = elems.filter(p)
      if elems1 == elems then this
      else Const(elems.filter(p))
    else
      this match
        case self: Filtered => Filtered(self.source, ref => self.p(ref) && p(ref))
        case _ => Filtered(asVar, p)

  /** Capture set obtained by applying `tm` to all elements of the current capture set
   *  and joining the results. If the current capture set is a variable we handle this as
   *  follows:
   *    - If the map is a BiTypeMap, the same transformation is applied to all
   *      future additions of new elements. We try to fuse with previous maps to
   *      avoid long paths of BiTypeMapped sets.
   *    - If the map is some other map that maps the current set of elements
   *      to itself, return the current var. We implicitly assume that the map
   *      will also map any elements added in the future to themselves. This assumption
   *      can be tested to hold by setting the ccConfig.checkSkippedMaps setting to true.
   *    - If the map is some other map that does not map all elements to themselves,
   *      freeze the current set (i.e. make it provisionally solved) and return
   *      the mapped elements as a constant set.
   */
  def map(tm: TypeMap)(using Context): CaptureSet =
    tm match
      case tm: BiTypeMap =>
        val mappedElems = elems.map(tm.mapCapability(_))
        if isConst then
          if mappedElems == elems then this
          else Const(mappedElems)
        else if ccState.mapFutureElems then
          def unfused = BiMapped(asVar, tm, mappedElems)
          this match
            case self: BiMapped => self.bimap.fuse(tm) match
              case Some(fused: BiTypeMap) => BiMapped(self.source, fused, mappedElems)
              case _ => unfused
            case _ => unfused
        else this
      case tm: IdentityCaptRefMap =>
        this
      case tm: AvoidMap if this.isInstanceOf[HiddenSet] =>
        this
      case _ =>
        val mapped = mapRefs(elems, tm, tm.variance)
        if mapped.elems == elems then
          if ccConfig.checkSkippedMaps && !isConst then asVar.skippedMaps += tm
          this
        else
          if !isConst then asVar.markSolved(provisional = true)
          mapped

  /** A mapping resulting from substituting parameters of a BindingType to a list of types */
  def substParams(tl: BindingType, to: List[Type])(using Context) =
    map(Substituters.SubstParamsMap(tl, to))

  def maybe(using Context): CaptureSet = map(MaybeMap())

  def readOnly(using Context): CaptureSet =
    val res = map(ReadOnlyMap())
    if mutability != Ignored then res.mutability = Reader
    res

  /** A bad root `elem` is inadmissible as a member of this set. What is a bad roots depends
   *  on the value of `rootLimit`.
   *  If the limit is null, all capture roots are good.
   *  If the limit is NoSymbol, all Fresh roots are good, but cap and Result roots are bad.
   *  If the limit is some other symbol, cap and Result roots are bad, as well as
   *  all Fresh roots that are contained (via ccOwner) in `rootLimit`.
   */
  protected def isBadRoot(rootLimit: Symbol | Null, elem: Capability)(using Context): Boolean =
    if rootLimit == null then false
    else elem.core match
      case GlobalCap | _: ResultCap => true
      case elem: FreshCap => elem.ccOwner.isContainedIn(rootLimit)
      case _ => false

  /** Invoke `handler` if this set has (or later aquires) a root capability.
   *  Excluded are Fresh instances unless their ccOwner is contained in `upto`.
   *  If `upto` is NoSymbol, all Fresh instances are admitted.
   */
  def disallowRootCapability(upto: Symbol)(handler: () => Context ?=> Unit)(using Context): this.type =
    if elems.exists(isBadRoot(upto, _)) then handler()
    this

  /** Invoke handler on the elements to ensure wellformedness of the capture set.
   *  The handler might add additional elements to the capture set.
   */
  def ensureWellformed(handler: Capability => Context ?=> Unit)(using Context): this.type =
    elems.foreach(handler(_))
    this

  /** An upper approximation of this capture set, i.e. a constant set that is
   *  subcaptured by this set. If the current set is a variable
   *  it is the intersection of all upper approximations of known supersets
   *  of the variable.
   *  The upper approximation is meaningful only if it is constant. If not,
   *  `upperApprox` can return an arbitrary capture set variable.
   *  `upperApprox` is used in `solve`.
   */
  protected def upperApprox(origin: CaptureSet)(using Context): CaptureSet

  /** Assuming set this set dependds on was just solved to be constant, propagate this info
   *  to this set. This might result in the set being solved to be constant
   *  itself.
   */
  protected def propagateSolved(provisional: Boolean)(using Context): Unit = ()

  /** This capture set with a description that tells where it comes from */
  def withDescription(description: String): CaptureSet

  /** The provided description (set via `withDescription`) for this capture set or else "" */
  def description: String

  /** More info enabled by -Y flags */
  def optionalInfo(using Context): String = ""

  /** A regular @retains or @retainsByName annotation with the elements of this set as arguments. */
  def toRegularAnnotation(cls: Symbol)(using Context): Annotation =
    Annotation(CaptureAnnotation(this, boxed = false)(cls).tree)

  override def toText(printer: Printer): Text =
    printer.toTextCaptureSet(this) ~~ description

  /** Apply function `f` to the elements. Typically used for printing.
   *  Overridden in HiddenSet so that we don't run into infinite recursions
   */
  def processElems[T](f: Refs => T): T = f(elems)

object CaptureSet:
  type Refs = SimpleIdentitySet[Capability]
  type Vars = SimpleIdentitySet[Var]
  type Deps = SimpleIdentitySet[CaptureSet]

  enum Mutability:
    case Mutable, Reader, Ignored

    def | (that: Mutability): Mutability =
      if this == that then this
      else if this == Ignored || that == Ignored then Ignored
      else if this == Reader || that == Reader then Reader
      else Mutable

    def & (that: Mutability): Mutability =
      if this == that then this
      else if this == Ignored then that
      else if that == Ignored then this
      else if this == Reader then that
      else this

  end Mutability
  import Mutability.*

  /** If set to `true`, capture stack traces that tell us where sets are created */
  private final val debugSets = false

  val emptyRefs: Refs = SimpleIdentitySet.empty

  /** The empty capture set `{}` */
  @sharable // sharable since the set is empty, so setMutable is a no-op
  val empty: CaptureSet.Const = Const(emptyRefs)

  /** The universal capture set `{cap}` */
  def universal(using Context): Const =
    Const(SimpleIdentitySet(GlobalCap))

  def fresh(origin: Origin)(using Context): Const =
    FreshCap(origin).singletonCaptureSet

  /** The shared capture set `{cap.rd}` */
  def shared(using Context): Const =
    GlobalCap.readOnly.singletonCaptureSet

  /** Used as a recursion brake */
  @sharable private[dotc] val Pending = Const(SimpleIdentitySet.empty)

  def apply(elems: Capability*)(using Context): Const =
    if elems.isEmpty then empty
    else
      for elem <- elems do
        assert(elem.isWellformed, i"not a trackable ref: $elem")
      Const(SimpleIdentitySet(elems*))

  def apply(elems: Refs)(using Context): Const =
    if elems.isEmpty then empty else Const(elems)

  /** The subclass of constant capture sets with given elements `elems` */
  class Const private[CaptureSet] (val elems: Refs, val description: String = "") extends CaptureSet:
    def isConst(using Context) = true
    def isAlwaysEmpty(using Context) = elems.isEmpty
    def isProvisionallySolved(using Context) = false

    def addThisElem(elem: Capability)(using Context, VarState): Boolean =
      addIfHiddenOrFail(elem)
      || {
        if this.isProvisionallySolved then
          capt.println(i"Cannot add $elem to provisionally solved $this")
        false
      }

    def toReader()(using Context) = failWith(MutAdaptFailure(this))

    def addDependent(cs: CaptureSet)(using Context, VarState) = true

    def upperApprox(origin: CaptureSet)(using Context): CaptureSet = this

    def withDescription(description: String): Const = Const(elems, description)

    def level = undefinedLevel

    def owner = NoSymbol

    private var isComplete = true

    def setMutable()(using Context): Unit =
      if !elems.isEmpty then
        isComplete = false // delay computation of Mutability status

    override def mutability(using Context): Mutability =
      if !isComplete then
        myMut = if maybeExclusive then Mutable else Reader
        isComplete = true
      myMut

    override def toString = elems.toString
  end Const

  case class EmptyWithProvenance(ref: Capability, mapped: CaptureSet) extends Const(SimpleIdentitySet.empty):
    override def optionalInfo(using Context): String =
      if ctx.settings.YccDebug.value
      then i" under-approximating the result of mapping $ref to $mapped"
      else ""

  /* The same as {cap.rd} but generated implicitly for references of Capability subtypes.
   */
  case class CSImpliedByCapability() extends Const(SimpleIdentitySet(GlobalCap.readOnly))

  /** A special capture set that gets added to the types of symbols that were not
   *  themselves capture checked, in order to admit arbitrary corresponding capture
   *  sets in subcapturing comparisons. Similar to platform types for explicit
   *  nulls, this provides more lenient checking against compilation units that
   *  were not yet compiled with capture checking on.
   */
  @sharable // sharable since the set is empty, so setMutable is a no-op
  object Fluid extends Const(emptyRefs):
    override def isAlwaysEmpty(using Context) = false
    override def addThisElem(elem: Capability)(using Context, VarState) = true
    override def toReader()(using Context) = true
    override def accountsFor(x: Capability)(using Context)(using VarState): Boolean = true
    override def mightAccountFor(x: Capability)(using Context): Boolean = true
    override def toString = "<fluid>"
  end Fluid

  /** The subclass of captureset variables with given initial elements */
  class Var(initialOwner: Symbol = NoSymbol, initialElems: Refs = emptyRefs, val level: Level = undefinedLevel, underBox: Boolean = false)(using @constructorOnly ictx: Context) extends CaptureSet:

    override def owner = initialOwner

    /** A unique identification number for diagnostics */
    val id =
      val ccs = ccState
      ccs.varId += 1
      ccs.varId

    override def maybeId = id

    //assert(id != 8, this)

    /** A variable is solved if it is aproximated to a from-then-on constant set.
     *  Interpretation:
     *    0              not solved
     *    Int.MaxValue   definitively solved
     *    n > 0          provisionally solved in iteration n
     */
    private var solved: Int = 0

    /** The elements currently known to be in the set */
    protected var myElems: Refs = initialElems

    def elems: Refs = myElems
    def elems_=(refs: Refs): Unit = myElems = refs

    /** The sets currently known to be dependent sets (i.e. new additions to this set
     *  are propagated to these dependent sets.)
     */
    var deps: Deps = SimpleIdentitySet.empty

    def setMutable()(using Context): Unit =
      mutability = Mutable

    def isConst(using Context) = solved >= ccState.iterationId
    def isAlwaysEmpty(using Context) = isConst && elems.isEmpty
    def isProvisionallySolved(using Context): Boolean = solved > 0 && solved != Int.MaxValue

    def isMaybeSet = false // overridden in BiMapped

    /** A handler to be invoked if the root reference `cap` is added to this set */
    var rootAddedHandler: () => Context ?=> Unit = () => ()

    /** The limit deciding which capture roots are bad (i.e. cannot be contained in this set).
     *  @see isBadRoot for details.
     */
    private[CaptureSet] var rootLimit: Symbol | Null = null

    /** A handler to be invoked when new elems are added to this set */
    var newElemAddedHandler: Capability => Context ?=> Unit = _ => ()

    var description: String = ""

    /** Check that all maps recorded in skippedMaps map `elem` to itself
     *  or something subsumed by it.
     */
    private def checkSkippedMaps(elem: Capability)(using Context): Unit =
      for tm <- skippedMaps do
        for elem1 <- mappedSet(elem, tm, variance = 1).elems do
          assert(elem.subsumes(elem1),
            i"Skipped map ${tm.getClass} maps newly added $elem to $elem1 in $this")

    protected def includeElem(elem: Capability)(using Context): Unit =
      if !elems.contains(elem) then
        elems += elem
        TypeComparer.logUndoAction: () =>
          elems -= elem

    def includeDep(cs: CaptureSet)(using Context): Unit =
      if !deps.contains(cs) then
        deps += cs
        TypeComparer.logUndoAction: () =>
          deps -= cs

    final def addThisElem(elem: Capability)(using Context, VarState): Boolean =
      if isConst || !varState.canRecord then // Fail if variable is solved or given VarState is frozen
        addIfHiddenOrFail(elem)
      else if !levelOK(elem) then
        failWith(IncludeFailure(this, elem, levelError = true))    // or `elem` is not visible at the level of the set.
      else
        // id == 108 then assert(false, i"trying to add $elem to $this")
        assert(elem.isWellformed, elem)
        assert(!this.isInstanceOf[HiddenSet] || summon[VarState].isSeparating, summon[VarState])
        includeElem(elem)
        if isBadRoot(rootLimit, elem) then
          rootAddedHandler()
        newElemAddedHandler(elem)
        val normElem = if isMaybeSet then elem else elem.stripMaybe
        // assert(id != 5 || elems.size != 3, this)
        val res = deps.forall: dep =>
          reporting.trace(i"forward $normElem from $this # $id to $dep # ${dep.maybeId} of class ${dep.getClass.toString}"):
            dep.tryInclude(normElem, this)
        if ccConfig.checkSkippedMaps && res then checkSkippedMaps(elem)
        if !res then
          elems -= elem
          TypeComparer.updateErrorNotes:
            case note: IncludeFailure => note.addToTrace(this)
        res

    final def toReader()(using Context) =
      if isConst then failWith(MutAdaptFailure(this))
      else
        mutability = Reader
        TypeComparer.logUndoAction(() => mutability = Mutable)
        deps.forall(_.mutableToReader(this))

    private def isPartOf(binder: Type)(using Context): Boolean =
      val find = new TypeAccumulator[Boolean]:
        def apply(b: Boolean, t: Type) =
          b || t.match
            case CapturingType(p, refs) => (refs eq Var.this) || this(b, p)
            case _ => foldOver(b, t)
      find(false, binder)

    private def levelOK(elem: Capability)(using Context): Boolean = elem match
      case _: FreshCap =>
        !level.isDefined
        || ccState.symLevel(elem.ccOwner) <= level
        || {
          capt.println(i"LEVEL ERROR $elem cannot be included in $this of $owner")
          false
        }
      case elem @ ResultCap(binder) =>
        rootLimit == null && (this.isInstanceOf[BiMapped] || isPartOf(binder.resType))
      case GlobalCap =>
        rootLimit == null
      case elem: TermRef if level.isDefined =>
        elem.prefix match
          case prefix: Capability =>
            levelOK(prefix)
          case _ =>
            ccState.symLevel(elem.symbol) <= level
      case elem: ThisType if level.isDefined =>
        ccState.symLevel(elem.cls).nextInner <= level
      case elem: ParamRef if !this.isInstanceOf[BiMapped] =>
        isPartOf(elem.binder.resType)
        || {
          capt.println(
            i"""LEVEL ERROR $elem for $this
                |elem binder = ${elem.binder}""")
          false
        }
      case elem: DerivedCapability =>
        levelOK(elem.underlying)
      case _ =>
        true

    def addDependent(cs: CaptureSet)(using Context, VarState): Boolean =
      (cs eq this)
      || cs.isUniversal
      || isConst
      || varState.canRecord && { includeDep(cs); true }

    override def disallowRootCapability(upto: Symbol)(handler: () => Context ?=> Unit)(using Context): this.type =
      rootLimit = upto
      rootAddedHandler = handler
      super.disallowRootCapability(upto)(handler)

    override def ensureWellformed(handler: Capability => (Context) ?=> Unit)(using Context): this.type =
      newElemAddedHandler = handler
      super.ensureWellformed(handler)

    private var computingApprox = false

    /** Roughly: the intersection of all constant known supersets of this set.
     *  The aim is to find an as-good-as-possible constant set that is a superset
     *  of this set. The universal set {cap} is a sound fallback.
     */
    final def upperApprox(origin: CaptureSet)(using Context): CaptureSet =
      if isConst then
        this
      else if isUniversal || computingApprox then
        universal
      else if containsCap && isReadOnly then
        shared
      else
        computingApprox = true
        try
          val approx = computeApprox(origin).ensuring(_.isConst)
          if approx.elems.exists(_.isInstanceOf[ResultCap]) then
            ccState.approxWarnings +=
                em"""Capture set variable $this gets upper-approximated
                  |to existential variable from $approx, using {cap} instead."""
            universal
          else approx
        finally computingApprox = false

    /** The intersection of all upper approximations of dependent sets */
    protected def computeApprox(origin: CaptureSet)(using Context): CaptureSet =
      ((universal: CaptureSet) /: deps) { (acc, sup) => acc ** sup.upperApprox(this) }

    /** Widen the variable's elements to its upper approximation and
     *  mark it as constant from now on. This is used for contra-variant type variables
     *  in the results of defs and vals.
     */
    def solve()(using Context): Unit =
      CCState.withCapAsRoot: // // OK here since we infer parameter types that get checked later
        val approx = upperApprox(empty)
          .map(CapToFresh(Origin.Unknown).inverse)    // Fresh --> cap
          .showing(i"solve $this = $result", capt)
        //println(i"solving var $this $approx ${approx.isConst} deps = ${deps.toList}")
        val newElems = approx.elems -- elems
        given VarState()
        if tryInclude(newElems, empty) then
          markSolved(provisional = false)

    /** Mark set as solved and propagate this info to all dependent sets */
    def markSolved(provisional: Boolean)(using Context): Unit =
      solved = if provisional then ccState.iterationId else Int.MaxValue
      deps.foreach(_.propagateSolved(provisional))
      if mutability == Mutable && !maybeExclusive then mutability = Reader


    var skippedMaps: Set[TypeMap] = Set.empty

    def withDescription(description: String): this.type =
      this.description = this.description.join(" and ", description)
      this

    /** Adds variables to the ShownVars context property if that exists, which
     *  establishes a record of all variables printed in an error message.
     *  Returns variable `ids` under -Ycc-debug, and owner/nesting level info
     *  under -Yprint-level.
     */
    override def optionalInfo(using Context): String =
      for vars <- ctx.property(ShownVars) do vars += this
      val debugInfo =
        if !ctx.settings.YccDebug.value then ""
        else if isConst then ids ++ "(solved)"
        else ids
      val limitInfo =
        if ctx.settings.YprintLevel.value && level.isDefined
        then i"<at level ${level.toString}>"
        else ""
      debugInfo ++ limitInfo

    /** Used for diagnostics and debugging: A string that traces the creation
     *  history of a variable by following source links. Each variable on the
     *  path is characterized by the variable's id and the first letter of the
     *  variable's class name. The path ends in a plain variable with letter `V` that
     *  is not derived from some other variable.
     */
    protected def ids(using Context): String =
      def descr = getClass.getSimpleName.nn.take(1)
      val trail = this.match
        case dv: DerivedVar =>
          def summary = if ctx.settings.YccVerbose.value then dv.summarize else descr
          s"$summary${dv.source.ids}"
        case _ => descr
      s"$id$trail"
    override def toString = s"Var$id$elems"
  end Var

  /** Variables that represent refinements of class parameters can have the universal
   *  capture set, since they represent only what is the result of the constructor.
   *  Test case: Without that tweak, logger.scala would not compile.
   */
  class RefiningVar(owner: Symbol)(using Context) extends Var(owner):
    override def disallowRootCapability(upto: Symbol)(handler: () => Context ?=> Unit)(using Context) = this

  /** A variable that is derived from some other variable via a map or filter. */
  abstract class DerivedVar(owner: Symbol, initialElems: Refs)(using @constructorOnly ctx: Context)
  extends Var(owner, initialElems):

    // For debugging: A trace where a set was created. Note that logically it would make more
    // sense to place this variable in Mapped, but that runs afoul of the initialization checker.
    // val stack = if debugSets && this.isInstanceOf[Mapped] then (new Throwable).getStackTrace().take(20) else null

    /** The variable from which this variable is derived */
    def source: Var

    mutability = source.mutability

    addAsDependentTo(source)

    override def mutableToReader(origin: CaptureSet)(using Context): Boolean =
      super.mutableToReader(origin)
      && ((origin eq source) || source.mutableToReader(this))

    override def propagateSolved(provisional: Boolean)(using Context) =
      if source.isConst && !isConst then markSolved(provisional)

    // ----------- Longest path recording -------------------------

    /** Summarize for set displaying in a path */
    def summarize: String = getClass.toString

    /** The length of the path of DerivedVars ending in this set */
    def pathLength: Int = source match
      case source: DerivedVar => source.pathLength + 1
      case _ => 1

    /** The path of DerivedVars ending in this set */
    def path: List[DerivedVar] = source match
      case source: DerivedVar => this :: source.path
      case _ => this :: Nil

    if ctx.settings.YccLog.value || util.Stats.enabled then
      ctx.run.nn.recordPath(pathLength, path)

  end DerivedVar

  /** A mapping where the type map is required to be a bijection.
   *  Parameters as in Mapped.
   */
  final class BiMapped private[CaptureSet]
    (val source: Var, val bimap: BiTypeMap, initialElems: Refs)(using @constructorOnly ctx: Context)
  extends DerivedVar(source.owner, initialElems):

    override def tryInclude(elem: Capability, origin: CaptureSet)(using Context, VarState): Boolean =
      if origin eq source then
        val mappedElem = bimap.mapCapability(elem)
        accountsFor(mappedElem) || addNewElem(mappedElem)
      else if accountsFor(elem) then
        true
      else
        // Propagate backwards to source. The element will be added then by another
        // forward propagation from source that hits the first branch `if origin eq source then`.
        try
          reporting.trace(i"prop backwards $elem from $this # $id to $source # ${source.id} via $summarize"):
            source.tryInclude(bimap.inverse.mapCapability(elem), this)
              .showing(i"propagating new elem $elem backward from $this/$id to $source = $result", captDebug)
        catch case ex: AssertionError =>
          println(i"fail while prop backwards tryInclude $elem of ${elem.getClass} from $this # $id / ${this.summarize} to $source # ${source.id}")
          throw ex

    /** For a BiTypeMap, supertypes of the mapped type also constrain
     *  the source via the inverse type mapping and vice versa. That is, if
     *   B = f(A) and B <: C, then A <: f^-1(C), so C should flow into
     *  the upper approximation of A.
     *  Conversely if A <: C2, then we also know that B <: f(C2).
     *  These situations are modeled by the two branches of the conditional below.
     */
    override def computeApprox(origin: CaptureSet)(using Context): CaptureSet =
      val supApprox = super.computeApprox(this)
      if source eq origin then supApprox.map(bimap.inverse)
      else source.upperApprox(this).map(bimap) ** supApprox

    override def isMaybeSet: Boolean = bimap.isInstanceOf[MaybeMap]
    override def toString = s"BiMapped$id($source, elems = $elems)"
    override def summarize = bimap.getClass.toString
  end BiMapped

  /** A variable with elements given at any time as { x <- source.elems | p(x) } */
  class Filtered private[CaptureSet]
    (val source: Var, val p: Context ?=> Capability => Boolean)(using @constructorOnly ctx: Context)
  extends DerivedVar(source.owner, source.elems.filter(p)):

    override def tryInclude(elem: Capability, origin: CaptureSet)(using Context, VarState): Boolean =
      if accountsFor(elem) then
        true
      else if origin eq source then
        !p(elem) || addNewElem(elem)
      else
        // Filtered elements have to be back-propagated to source.
        // Elements that don't satisfy `p` are not allowed.
        if p(elem) then source.tryInclude(elem, this)
        else failWith(IncludeFailure(this, elem))

    override def computeApprox(origin: CaptureSet)(using Context): CaptureSet =
      if source eq origin then
        // it's a filter of origin, so not a superset of `origin`,
        // therefore don't contribute to the intersection.
        universal
      else
        source.upperApprox(this).filter(p)

    override def toString = s"${getClass.getSimpleName}$id($source, elems = $elems)"
  end Filtered

  /** A variable with elements given at any time as { x <- source.elems | !other.accountsFor(x) } */
  class Diff(source: Var, other: Const)(using Context)
  extends Filtered(source, !other.accountsFor(_))

  class Union(cs1: CaptureSet, cs2: CaptureSet)(using Context)
  extends Var(initialElems = cs1.elems ++ cs2.elems):
    addAsDependentTo(cs1)
    addAsDependentTo(cs2)
    mutability = cs1.mutability | cs2.mutability

    override def tryInclude(elem: Capability, origin: CaptureSet)(using Context, VarState): Boolean =
      if accountsFor(elem) then true
      else
        val res = super.tryInclude(elem, origin)
        // If this is the union of a constant and a variable,
        // propagate `elem` to the variable part to avoid slack
        // between the operands and the union.
        if res && (origin ne cs1) && (origin ne cs2) then
          if cs1.isConst then cs2.tryInclude(elem, origin)
          else if cs2.isConst then cs1.tryInclude(elem, origin)
          else res
        else res

    override def mutableToReader(origin: CaptureSet)(using Context): Boolean =
      super.mutableToReader(origin)
      && {
        if (origin eq cs1) || (origin eq cs2) then true
        else if cs1.isConst && cs1.mutability == Mutable then cs2.mutableToReader(this)
        else if cs2.isConst && cs2.mutability == Mutable then cs1.mutableToReader(this)
        else true
      }

    override def propagateSolved(provisional: Boolean)(using Context) =
      if cs1.isConst && cs2.isConst && !isConst then markSolved(provisional)
  end Union

  class Intersection(cs1: CaptureSet, cs2: CaptureSet)(using Context)
  extends Var(initialElems = elemIntersection(cs1, cs2)):
    addAsDependentTo(cs1)
    addAsDependentTo(cs2)
    deps += cs1
    deps += cs2
    mutability = cs1.mutability & cs2.mutability

    override def tryInclude(elem: Capability, origin: CaptureSet)(using Context, VarState): Boolean =
      val inIntersection =
        if origin eq cs1 then cs2.accountsFor(elem)
        else if origin eq cs2 then cs1.accountsFor(elem)
        else true
      !inIntersection
      || accountsFor(elem)
      || addNewElem(elem)
        && ((origin eq cs1) || cs1.tryInclude(elem, this))
        && ((origin eq cs2) || cs2.tryInclude(elem, this))

    override def mutableToReader(origin: CaptureSet)(using Context): Boolean =
      super.mutableToReader(origin)
      && ((origin eq cs1) || cs1.mutableToReader(this))
      && ((origin eq cs2) || cs2.mutableToReader(this))

    override def computeApprox(origin: CaptureSet)(using Context): CaptureSet =
      if (origin eq cs1) || (origin eq cs2) then
        // it's a combination of origin with some other set, so not a superset of `origin`,
        // therefore don't contribute to the intersection.
        universal
      else
        CaptureSet(elemIntersection(cs1.upperApprox(this), cs2.upperApprox(this)))

    override def propagateSolved(provisional: Boolean)(using Context) =
      if cs1.isConst && cs2.isConst && !isConst then markSolved(provisional)
  end Intersection

  def elemIntersection(cs1: CaptureSet, cs2: CaptureSet)(using Context): Refs =
    cs1.elems.filter(cs2.accountsFor) ++ cs2.elems.filter(cs1.accountsFor)

  /** A capture set variable used to record the references hidden by a Fresh instance,
   *  The elems and deps members are repurposed as follows:
   *    elems: Set of hidden references
   *    deps : Set of hidden sets for which the Fresh instance owning this set
   *           is a hidden element.
   *  Hidden sets may become aliases of other hidden sets, which means that
   *  reads and writes of elems go to the alias.
   *  If H is an alias of R.hidden for some Fresh instance R then:
   *    H.elems == {R}
   *    H.deps = {R.hidden}
   *  This encoding was chosen because it relies only on the elems and deps fields
   *  which are already subject through snapshotting and rollbacks in VarState.
   *  It's advantageous if we don't need to deal with other pieces of state there.
   */
  class HiddenSet(initialOwner: Symbol, val owningCap: FreshCap)(using @constructorOnly ictx: Context)
  extends Var(initialOwner):
    var givenOwner: Symbol = initialOwner

    override def owner = givenOwner

    //assert(id != 3)

    description = i"of elements subsumed by a fresh cap in $initialOwner"

    private def aliasRef: FreshCap | Null =
      if myElems.size == 1 then
        myElems.nth(0) match
          case alias: FreshCap if deps.contains(alias.hiddenSet) => alias
          case _ => null
      else null

    private def aliasSet: HiddenSet =
      if myElems.size == 1 then
        myElems.nth(0) match
          case alias: FreshCap if deps.contains(alias.hiddenSet) => alias.hiddenSet
          case _ => this
      else this

    def superCaps: List[FreshCap] =
      deps.toList.map(_.asInstanceOf[HiddenSet].owningCap)

    override def elems: Refs =
      val al = aliasSet
      if al eq this then super.elems else al.elems

    override def elems_=(refs: Refs) =
      val al = aliasSet
      if al eq this then super.elems_=(refs) else al.elems_=(refs)

    /** Add element to hidden set. Also add it to all supersets (as indicated by
     *  deps of this set). Follow aliases on both hidden set and added element
     *  before adding. If the added element is also a Fresh instance with
     *  hidden set H which is a superset of this set, then make this set an
     *  alias of H.
     */
    def add(elem: Capability)(using ctx: Context, vs: VarState): Unit =
      val alias = aliasSet
      if alias ne this then alias.add(elem)
      else
        def addToElems() =
          includeElem(elem)
          deps.foreach: dep =>
            assert(dep != this)
            vs.addHidden(dep.asInstanceOf[HiddenSet], elem)
        elem match
          case elem: FreshCap =>
            if this ne elem.hiddenSet then
              val alias = elem.hiddenSet.aliasRef
              if alias != null then
                add(alias)
              else if deps.contains(elem.hiddenSet) then // make this an alias of elem
                capt.println(i"Alias $this to ${elem.hiddenSet}")
                elems = SimpleIdentitySet(elem)
                deps = SimpleIdentitySet(elem.hiddenSet)
              else
                addToElems()
                elem.hiddenSet.includeDep(this)
          case _ =>
            addToElems()

    /** Apply function `f` to `elems` while setting `elems` to empty for the
     *  duration. This is used to escape infinite recursions if two Freshs
     *  refer to each other in their hidden sets.
     */
    override def processElems[T](f: Refs => T): T =
      val savedElems = elems
      elems = emptyRefs
      try f(savedElems)
      finally elems = savedElems
  end HiddenSet

  /** Extrapolate tm(r) according to `variance`. Let r1 be the result of tm(r).
   *    - If r1 is a tracked capability, return {r1}
   *    - If r1 has an empty capture set, return {}
   *    - Otherwise,
   *        - if the variance is covariant, return r1's capture set
   *        - if the variance is contravariant, return {}
   *        - Otherwise assertion failure
   */
  final def mappedSet(r: Capability, tm: TypeMap, variance: Int)(using Context): CaptureSet =
    tm.mapCapability(r) match
      case c: CoreCapability => c.captureSet
      case c: Capability => c.singletonCaptureSet
      case (cs: CaptureSet, exact) =>
        if cs.isAlwaysEmpty || exact || variance > 0 then cs
        else if variance < 0 then CaptureSet.EmptyWithProvenance(r, cs)
        else cs.maybe

  /** Apply `f` to each element in `xs`, and join result sets with `++` */
  def mapRefs(xs: Refs, f: Capability => CaptureSet)(using Context): CaptureSet =
    ((empty: CaptureSet) /: xs)((cs, x) => cs ++ f(x))

  /** Apply extrapolated `tm` to each element in `xs`, and join result sets with `++` */
  def mapRefs(xs: Refs, tm: TypeMap, variance: Int)(using Context): CaptureSet =
    mapRefs(xs, mappedSet(_, tm, variance))

  /** Return true iff
   *   - arg1 is a TypeBounds >: CL T <: CH T of two capturing types with equal parents.
   *   - arg2 is a capturing type CA U
   *   - CH <: CA <: CL
   *  In other words, we can unify CL, CH and CA.
   */
  def subCapturesRange(arg1: TypeBounds, arg2: Type)(using Context): Boolean = arg1 match
    case TypeBounds(CapturingType(lo, loRefs), CapturingType(hi, hiRefs)) if lo =:= hi =>
      given VarState()
      val cs2 = arg2.captureSet
      hiRefs.subCaptures(cs2) && cs2.subCaptures(loRefs)
    case _ =>
      false

  /** A TypeMap that is the identity on capabilities */
  trait IdentityCaptRefMap extends TypeMap

  /** A value of this class is produced and added as a note to ccState
   *  when a subsumes check decides that an existential variable `ex` cannot be
   *  instantiated to the other capability `other`.
   */
  case class ExistentialSubsumesFailure(val ex: ResultCap, val other: Capability) extends ErrorNote

  /** Failure indicating that `elem` cannot be included in `cs` */
  case class IncludeFailure(cs: CaptureSet, elem: Capability, levelError: Boolean = false) extends ErrorNote, Showable:
    private var myTrace: List[CaptureSet] = cs :: Nil

    def trace: List[CaptureSet] = myTrace
    def addToTrace(cs1: CaptureSet) =
      val res = IncludeFailure(cs, elem, levelError)
      res.myTrace = cs1 :: this.myTrace
      res

    override def toText(printer: Printer): Text =
      inContext(printer.printerContext):
        if levelError then
          i"($elem at wrong level for $cs at level ${cs.level.toString})"
        else
          if ctx.settings.YccDebug.value
          then i"$elem cannot be included in $trace"
          else i"$elem cannot be included in $cs"
  end IncludeFailure

  /** Failure indicating that a read-only capture set of a mutable type cannot be
   *  widened to an exclusive set.
   *  @param  cs    the exclusive set in question
   *  @param  lo    the lower type of the orginal type comparison, or NoType if not known
   *  @param  hi    the upper type of the orginal type comparison, or NoType if not known
   */
  case class MutAdaptFailure(cs: CaptureSet, lo: Type = NoType, hi: Type = NoType) extends ErrorNote

  /** A VarState serves as a snapshot mechanism that can undo
   *  additions of elements or super sets if an operation fails
   */
  class VarState:

    /** A map from ResultCap values to other ResultCap values. If two result values
     *  `a` and `b` are unified, then `eqResultMap(a) = b` and `eqResultMap(b) = a`.
     */
    private var eqResultMap: util.SimpleIdentityMap[ResultCap, ResultCap] = util.SimpleIdentityMap.empty

    /** Record elements, return whether this was allowed.
     *  By default, recording is allowed in regular but not in frozen states.
     */
    def canRecord: Boolean = true

    /** Does this state allow additions of elements to capture set variables? */
    def isOpen = true
    def isSeparating = false

    /** Add element to hidden set, recording it in elemsMap,
     *  return whether this was allowed. By default, recording is allowed
     *  but the special state VarState.Separate overrides this.
     */
    def addHidden(hidden: HiddenSet, elem: Capability)(using Context): Boolean =
      hidden.add(elem)(using ctx, this)
      true

    /** If root1 and root2 belong to the same binder but have different originalBinders
     *  it means that one of the roots was mapped to the binder of the other by a
     *  substBinder when comparing two method types. In that case we can unify
     *  the two roots1, provided none of the two roots have already been unified
     *  themselves. So unification must be 1-1.
     *
     *  Note, see (**) below: We also allow unifications of results that have different ExprType
     *  binders. This is necessary because ExprTypes don't get updated with SubstBindingMaps.
     *  It's sound since ExprTypes always appear alone and at the top-level, so there is
     *  no problem with confusing results at different levels.
     *  See pos-customargs/captures/overrides.scala for a test case.
     */
    def unify(c1: ResultCap, c2: ResultCap)(using Context): Boolean =
      ((c1.binder eq c2.binder)
        || c1.binder.isInstanceOf[ExprType] && c2.binder.isInstanceOf[ExprType] // (**)
      )
      && (c1.originalBinder ne c2.originalBinder)
      && eqResultMap(c1) == null
      && eqResultMap(c2) == null
      && {
        eqResultMap = eqResultMap.updated(c1, c2).updated(c2, c1)
        TypeComparer.logUndoAction: () =>
          eqResultMap.remove(c1)
          eqResultMap.remove(c2)
        true
      }

    private var seen: util.EqHashSet[Capability] = new util.EqHashSet

    /** Run test `pred` unless `ref` was seen in an enclosing `ifNotSeen` operation */
    def ifNotSeen(ref: Capability)(pred: => Boolean): Boolean =
      if seen.add(ref) then
        try pred finally seen -= ref
      else false

    override def toString = "open varState"

  object VarState:

    /** A class for states that do not allow to record elements or dependent sets.
     *  In effect this means that no new elements or dependent sets can be added
     *  in these states (since the previous state cannot be recorded in a snapshot)
     *  On the other hand, these states do allow by default Fresh instances to
     *  subsume arbitary types, which are then recorded in their hidden sets.
     */
    class Closed extends VarState:
      override def canRecord = false
      override def isOpen = false
      override def toString = "closed varState"

    /** A closed state that allows a Fresh instance to subsume a
     *  reference `r` only if `r` is already present in the hidden set of the instance.
     *  No new references can be added.
     */
    class Separating extends Closed:
      override def addHidden(hidden: HiddenSet, elem: Capability)(using Context): Boolean = false
      override def toString = "separating varState"
      override def isSeparating = true

    /** A closed state that allows a Fresh instance to subsume a
     *  reference `r` only if `r` is already present in the hidden set of the instance.
     *  No new references can be added.
     */
    def Separate(using Context): Separating = ccState.Separate

    /** Like Separate but in addition we assume that `cap` never subsumes anything else.
     *  Used in `++` to not lose track of dependencies between function parameters.
     */
    def HardSeparate(using Context): Separating = ccState.HardSeparate

    /** A mixin trait that overrides the addHidden and unify operations to
     *  not depend in state. */
    trait Stateless extends VarState:

      /** Allow adding hidden elements, but don't store them */
      override def addHidden(hidden: HiddenSet, elem: Capability)(using Context): Boolean = true

      /** Don't allow to unify result caps */
      override def unify(c1: ResultCap, c2: ResultCap)(using Context): Boolean = false
    end Stateless

    /** An open state that turns off recording of hidden elements (but allows
     *  adding them). Used in `addAsDependentTo`. Instantiated in ccState.Unrecorded.
     */
    class Unrecorded extends VarState, Stateless:
      override def toString = "unrecorded varState"

    def Unrecorded(using Context): Unrecorded = ccState.Unrecorded

    /** A closed state that turns off recording of hidden elements (but allows
     *  adding them). Used in `mightAccountFor`. Instantiated in ccState.ClosedUnrecorded.
     */
    class ClosedUnrecorded extends Closed, Stateless:
      override def toString = "closed unrecorded varState"

    def ClosedUnrecorded(using Context): ClosedUnrecorded = ccState.ClosedUnrecorded

  end VarState

  /** The current VarState, as passed by the implicit context */
  def varState(using state: VarState): VarState = state

  /** A template for maps on capabilities where f(c) <: c and f(f(c)) = c */
  private abstract class NarrowingCapabilityMap(using Context) extends BiTypeMap:

    def apply(t: Type) = mapOver(t)

    override def fuse(next: BiTypeMap)(using Context) = next match
      case next: Inverse if next.inverse.getClass == getClass => Some(IdentityTypeMap)
      case next: NarrowingCapabilityMap if next.getClass == getClass => Some(this)
      case _ => None

    class Inverse extends BiTypeMap:
      def apply(t: Type) = t // since f(c) <: c, this is the best inverse
      override def mapCapability(c: Capability, deep: Boolean): Capability = c
      def inverse = NarrowingCapabilityMap.this
      override def toString = NarrowingCapabilityMap.this.toString ++ ".inverse"
      override def fuse(next: BiTypeMap)(using Context) = next match
        case next: NarrowingCapabilityMap if next.inverse.getClass == getClass => Some(IdentityTypeMap)
        case next: NarrowingCapabilityMap if next.getClass == getClass => Some(this)
        case _ => None

    lazy val inverse = Inverse()
  end NarrowingCapabilityMap

  /** Maps `x` to `x?` */
  private class MaybeMap(using Context) extends NarrowingCapabilityMap:
    override def mapCapability(c: Capability, deep: Boolean) = c.maybe
    override def toString = "Maybe"

  /** Maps `x` to `x.rd` */
  private class ReadOnlyMap(using Context) extends NarrowingCapabilityMap:
    override def mapCapability(c: Capability, deep: Boolean) = c.readOnly
    override def toString = "ReadOnly"

  /* Not needed:
  def ofClass(cinfo: ClassInfo, argTypes: List[Type])(using Context): CaptureSet =
    CaptureSet.empty
      def captureSetOf(tp: Type): CaptureSet = tp match
        case tp: TypeRef if tp.symbol.is(ParamAccessor) =>
          def mapArg(accs: List[Symbol], tps: List[Type]): CaptureSet = accs match
            case acc :: accs1 if tps.nonEmpty =>
              if acc == tp.symbol then tps.head.captureSet
              else mapArg(accs1, tps.tail)
            case _ =>
              empty
          mapArg(cinfo.cls.paramAccessors, argTypes)
        case _ =>
          tp.captureSet
      val css =
        for
          parent <- cinfo.parents if parent.classSymbol == defn.RetainingClass
          arg <- parent.argInfos
        yield captureSetOf(arg)
      css.foldLeft(empty)(_ ++ _)
  */

  /** The capture set of the type underlying the capability `c` */
  def ofInfo(c: Capability)(using Context): CaptureSet = c match
    case Reach(c1) =>
      c1.widen.deepCaptureSet(includeTypevars = true)
        .showing(i"Deep capture set of $c: ${c1.widen} = ${result}", capt)
    case ReadOnly(c1) =>
      c1.captureSetOfInfo.readOnly
    case Maybe(c1) =>
      c1.captureSetOfInfo.maybe
    case c: RootCapability =>
      c.singletonCaptureSet
    case c: ParamRef if !c.underlying.exists =>
      // might happen during construction of lambdas, assume `{cap}` in this case so that
      // `ref` will not seem subsumed by other capabilities in a `++`.
      universal
    case c: CoreCapability =>
      ofType(c.underlying, followResult = false)

  /** Capture set of a type
   *  @param followResult  If true, also include capture sets of function results.
   *                       This mode is currently not used. It could be interesting
   *                       when we change the system so that the capture set of a function
   *                       is the union of the capture sets if its span.
   *                       In this case we should use `followResult = true` in the call in ofInfo above.
   */
  def ofType(tp: Type, followResult: Boolean)(using Context): CaptureSet =
    def recur(tp: Type): CaptureSet = trace(i"ofType $tp, ${tp.getClass} $followResult", show = true):
      tp.dealiasKeepAnnots match
        case tp: TermRef =>
          tp.captureSet
        case tp: TermParamRef =>
          tp.captureSet
        case tp: (TypeRef | TypeParamRef) =>
          if tp.derivesFrom(defn.Caps_CapSet) then tp.captureSet
          else empty
        case CapturingType(parent, refs) =>
          recur(parent) ++ refs
        case tp @ AnnotatedType(parent, ann) if ann.symbol.isRetains =>
          recur(parent) ++ ann.tree.toCaptureSet
        case tpd @ defn.RefinedFunctionOf(rinfo: MethodType) if followResult =>
          ofType(tpd.parent, followResult = false)            // pick up capture set from parent type
          ++ recur(rinfo.resType)                             // add capture set of result
              .filter:
                case TermParamRef(binder, _) => binder ne rinfo
                case ResultCap(binder) => binder ne rinfo
                case _ => true
        case tpd @ AppliedType(tycon, args) =>
          if followResult && defn.isNonRefinedFunction(tpd) then
            recur(args.last)
              // must be (pure) FunctionN type since ImpureFunctions have already
              // been eliminated in selector's dealias. Use capture set of result.
          else
            val cs = recur(tycon)
            tycon.typeParams match
              case tparams @ (LambdaParam(tl, _) :: _) => cs.substParams(tl, args)
              case _ => cs
        case tp: TypeProxy =>
          recur(tp.superType)
        case AndType(tp1, tp2) =>
          recur(tp1) ** recur(tp2)
        case OrType(tp1, tp2) =>
          recur(tp1) ++ recur(tp2)
        case _ =>
          empty
    recur(tp)
      //.showing(i"capture set of $tp = $result", captDebug)

  /** The deep capture set of a type is the union of all covariant occurrences of
   *  capture sets. Nested existential sets are approximated with `cap`.
   */
  def ofTypeDeeply(tp: Type, includeTypevars: Boolean = false)(using Context): CaptureSet =
    val collect = new DeepTypeAccumulator[CaptureSet]:
      def capturingCase(acc: CaptureSet, parent: Type, refs: CaptureSet) =
        this(acc, parent) ++ refs
      def abstractTypeCase(acc: CaptureSet, t: TypeRef, upperBound: Type) =
        if includeTypevars && upperBound.isExactlyAny then fresh(Origin.DeepCS(t))
        else this(acc, upperBound)
    collect(CaptureSet.empty, tp)

  type AssumedContains = immutable.Map[TypeRef, SimpleIdentitySet[Capability]]
  val AssumedContains: Property.Key[AssumedContains] = Property.Key()

  def assumedContains(using Context): AssumedContains =
    ctx.property(AssumedContains).getOrElse(immutable.Map.empty)

  private val ShownVars: Property.Key[mutable.Set[Var]] = Property.Key()

  /** Perform `op`. Under -Ycc-debug, collect and print info about all variables reachable
   *  via `(_.deps)*` from the variables that were shown in `op`.
   */
  def withCaptureSetsExplained[T](op: Context ?=> T)(using ctx: Context): T =
    if ctx.settings.YccDebug.value then
      val shownVars = mutable.Set[Var]()
      inContext(ctx.withProperty(ShownVars, Some(shownVars))) {
        try op
        finally
          val reachable = mutable.Set[Var]()
          val todo = mutable.Queue[Var]() ++= shownVars
          def incl(cv: Var): Unit =
            if !reachable.contains(cv) then todo += cv
          while todo.nonEmpty do
            val cv = todo.dequeue()
            if !reachable.contains(cv) then
              reachable += cv
              cv.deps.foreach {
                case cv: Var => incl(cv)
                case _ =>
              }
              cv match
                case cv: DerivedVar => incl(cv.source)
                case _ =>
          val allVars = reachable.toArray.sortBy(_.id)
          println(i"Capture set dependencies:")
          for cv <- allVars do
            println(i"  ${cv.show.padTo(20, ' ')} :: ${cv.deps.toList}%, %")
      }
    else op
end CaptureSet
