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
import CCState.*

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
 *    cs1 = ∪ {f(x) | x ∈ cs2}     where f is a function from capture references to capture sets.
 *    cs1 = ∪ {x | x ∈ cs2, p(x)}  where p is a predicate on capture references
 *    cs1 = cs2 ∩ cs2
 *
 *  We call the resulting constraint system "monadic set constraints".
 *  To support capture propagation across maps, mappings are supported only
 *  if the mapped function is either a bijection or if it is idempotent
 *  on capture references (c.f. doc comment on `map` below).
 */
sealed abstract class CaptureSet extends Showable:
  import CaptureSet.*

  /** The elements of this capture set. For capture variables,
   *  the elements known so far.
   */
  def elems: Refs

  /** Is this capture set constant (i.e. not an unsolved capture variable)?
   *  Solved capture variables count as constant.
   */
  def isConst: Boolean

  /** Is this capture set always empty? For unsolved capture veriables, returns
   *  always false.
   */
  def isAlwaysEmpty: Boolean

  /** An optional level limit, or undefinedLevel if none exists. All elements of the set
   *  must be at levels equal or smaller than the level of the set, if it is defined.
   */
  def level: Level

  /** An optional owner, or NoSymbol if none exists. Used for diagnstics
   */
  def owner: Symbol

  /** Is this capture set definitely non-empty? */
  final def isNotEmpty: Boolean = !elems.isEmpty

  /** Convert to Const. @pre: isConst */
  def asConst: Const = this match
    case c: Const => c
    case v: Var =>
      assert(v.isConst)
      Const(v.elems)

  /** Cast to variable. @pre: !isConst */
  def asVar: Var =
    assert(!isConst)
    asInstanceOf[Var]

  /** Convert to Const with current elements unconditionally */
  def toConst: Const = this match
    case c: Const => c
    case v: Var => Const(v.elems)

  /** Does this capture set contain the root reference `cap` as element? */
  final def isUniversal(using Context) =
    elems.exists(_.isCap)

  /** Does this capture set contain the root reference `cap` as element? */
  final def isUniversalOrFresh(using Context) =
    elems.exists(_.isCapOrFresh)

  /** Does this capture set contain a root reference `cap` or `cap.rd` as element? */
  final def containsRootCapability(using Context) =
    elems.exists(_.isRootCapability)

  final def containsCap(using Context) =
    elems.exists(_.stripReadOnly.isCap)

  final def isUnboxable(using Context) =
    elems.exists(elem => elem.isRootCapability || Existential.isExistentialVar(elem))

  final def isReadOnly(using Context): Boolean =
    elems.forall(_.isReadOnly)

  final def isExclusive(using Context): Boolean =
    elems.exists(_.isExclusive)

  final def keepAlways: Boolean = this.isInstanceOf[EmptyWithProvenance]

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
  protected def tryInclude(elem: CaptureRef, origin: CaptureSet)(using Context, VarState): CompareResult =
    if accountsFor(elem) then CompareResult.OK
    else addNewElem(elem)

  /** Try to include all element in `refs` to this capture set. */
  protected final def tryInclude(newElems: Refs, origin: CaptureSet)(using Context, VarState): CompareResult =
    (CompareResult.OK /: newElems): (r, elem) =>
      r.andAlso(tryInclude(elem, origin))

  /** Add an element to this capture set, assuming it is not already accounted for,
   *  and omitting any mapping or filtering.
   *
   *  If the element itself cannot be added to the set for some reason, and the
   *  element is not the root capability, try instead to include its underlying
   *  capture set.
   */
  protected final def addNewElem(elem: CaptureRef)(using ctx: Context, vs: VarState): CompareResult =
    if elem.isMaxCapability || !vs.isOpen then
      addThisElem(elem)
    else
      addThisElem(elem).orElse:
        val underlying = elem.captureSetOfInfo
        tryInclude(underlying.elems, this).andAlso:
          underlying.addDependent(this)
          CompareResult.OK

  /** Add new elements one by one using `addNewElem`, abort on first failure */
  protected final def addNewElems(newElems: Refs)(using Context, VarState): CompareResult =
    (CompareResult.OK /: newElems): (r, elem) =>
      r.andAlso(addNewElem(elem))

  /** Add a specific element, assuming it is not already accounted for,
   *  and omitting any mapping or filtering, without possibility to backtrack
   *  to the underlying capture set.
   */
  protected def addThisElem(elem: CaptureRef)(using Context, VarState): CompareResult

  protected def addHiddenElem(elem: CaptureRef)(using ctx: Context, vs: VarState): CompareResult =
    if elems.exists(_.maxSubsumes(elem, canAddHidden = true))
    then CompareResult.OK
    else CompareResult.Fail(this :: Nil)

  /** If this is a variable, add `cs` as a dependent set */
  protected def addDependent(cs: CaptureSet)(using Context, VarState): CompareResult

  /** If `cs` is a variable, add this capture set as one of its dependent sets */
  protected def addAsDependentTo(cs: CaptureSet)(using Context): this.type =
    cs.addDependent(this)(using ctx, VarState.Unrecorded)
    this

  /** {x} <:< this   where <:< is subcapturing, but treating all variables
   *                 as frozen.
   */
  def accountsFor(x: CaptureRef)(using ctx: Context, vs: VarState = VarState.Separate): Boolean =

    def debugInfo(using Context) = i"$this accountsFor $x, which has capture set ${x.captureSetOfInfo}"

    def test(using Context) = reporting.trace(debugInfo):
      elems.exists(_.subsumes(x))
      || // Even though subsumes already follows captureSetOfInfo, this is not enough.
         // For instance x: C^{y, z}. Then neither y nor z subsumes x but {y, z} accounts for x.
        !x.isMaxCapability
        && !x.derivesFrom(defn.Caps_CapSet)
        && !(vs == VarState.Separate && x.captureSetOfInfo.containsRootCapability)
           // in VarState.Separate, don't try to widen to cap since that might succeed with {cap} <: {cap}
        && x.captureSetOfInfo.subCaptures(this, VarState.Separate).isOK

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
  def mightAccountFor(x: CaptureRef)(using Context): Boolean =
    reporting.trace(i"$this mightAccountFor $x, ${x.captureSetOfInfo}?", show = true):
      elems.exists(_.subsumes(x)(using ctx, VarState.ClosedUnrecorded))
      || !x.isMaxCapability
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

  /** The subcapturing test.
   *  @param frozen   if true, no new variables or dependent sets are allowed to
   *                  be added when making this test. An attempt to add either
   *                  will result in failure.
   */
  final def subCaptures(that: CaptureSet, vs: VarState)(using Context): CompareResult =
    subCaptures(that)(using ctx, vs)

  /** The subcapturing test, using a given VarState */
  final def subCaptures(that: CaptureSet)(using ctx: Context, vs: VarState = VarState()): CompareResult =
    val result = that.tryInclude(elems, this)
    if result.isOK then
      addDependent(that)
    else
      ccState.levelError = ccState.levelError.orElse(result.levelError)
      varState.rollBack()
      result
      //.showing(i"subcaptures $this <:< $that = ${result.show}", capt)

  /** Two capture sets are considered =:= equal if they mutually subcapture each other
   *  in a frozen state.
   */
  def =:= (that: CaptureSet)(using Context): Boolean =
       this.subCaptures(that, VarState.Separate).isOK
    && that.subCaptures(this, VarState.Separate).isOK

  /** The smallest capture set (via <:<) that is a superset of both
   *  `this` and `that`
   */
  def ++ (that: CaptureSet)(using Context): CaptureSet =
    if this.subCaptures(that, VarState.Separate).isOK then
      if that.isAlwaysEmpty && this.keepAlways then this else that
    else if that.subCaptures(this, VarState.Separate).isOK then this
    else if this.isConst && that.isConst then Const(this.elems ++ that.elems)
    else Union(this, that)

  def ++ (that: CaptureSet.Const)(using Context): CaptureSet.Const =
    Const(this.elems ++ that.elems)

  /** The smallest superset (via <:<) of this capture set that also contains `ref`.
   */
  def + (ref: CaptureRef)(using Context): CaptureSet =
    this ++ ref.singletonCaptureSet

  /** The largest capture set (via <:<) that is a subset of both `this` and `that`
   */
  def **(that: CaptureSet)(using Context): CaptureSet =
    if this.subCaptures(that, VarState.Closed()).isOK then this
    else if that.subCaptures(this, VarState.Closed()).isOK then that
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
  def - (ref: CaptureRef)(using Context): CaptureSet =
    this -- ref.singletonCaptureSet

  /** The largest subset (via <:<) of this capture set that only contains elements
   *  for which `p` is true.
   */
  def filter(p: Context ?=> CaptureRef => Boolean)(using Context): CaptureSet =
    if this.isConst then
      val elems1 = elems.filter(p)
      if elems1 == elems then this
      else Const(elems.filter(p))
    else Filtered(asVar, p)

  /** Capture set obtained by applying `tm` to all elements of the current capture set
   *  and joining the results. If the current capture set is a variable, the same
   *  transformation is applied to all future additions of new elements.
   *
   *  Note: We have a problem how we handle the situation where we have a mapped set
   *
   *    cs2 = tm(cs1)
   *
   *  and then the propagation solver adds a new element `x` to `cs2`. What do we
   *  know in this case about `cs1`? We can answer this question in a sound way only
   *  if `tm` is a bijection on capture references or it is idempotent on capture references.
   *  (see definition in IdempotentCapRefMap).
   *  If `tm` is a bijection we know that `tm^-1(x)` must be in `cs1`. If `tm` is idempotent
   *  one possible solution is that `x` is in `cs1`, which is what we assume in this case.
   *  That strategy is sound but not complete.
   *
   *  If `tm` is some other map, we don't know how to handle this case. For now,
   *  we simply refuse to handle other maps. If they do need to be handled,
   *  `OtherMapped` provides some approximation to a solution, but it is neither
   *  sound nor complete.
   */
  def map(tm: TypeMap)(using Context): CaptureSet = tm match
    case tm: BiTypeMap =>
      val mappedElems = elems.map(tm.forward)
      if isConst then
        if mappedElems == elems then this
        else Const(mappedElems)
      else BiMapped(asVar, tm, mappedElems)
    case tm: IdentityCaptRefMap =>
      this
    case _ =>
      val mapped = mapRefs(elems, tm, tm.variance)
      if isConst then
        if mapped.isConst && mapped.elems == elems && !mapped.keepAlways then this
        else mapped
      else Mapped(asVar, tm, tm.variance, mapped)

  /** A mapping resulting from substituting parameters of a BindingType to a list of types */
  def substParams(tl: BindingType, to: List[Type])(using Context) =
    map(Substituters.SubstParamsMap(tl, to))

  def maybe(using Context): CaptureSet = map(MaybeMap())

  def readOnly(using Context): CaptureSet = map(ReadOnlyMap())

  /** Invoke handler if this set has (or later aquires) the root capability `cap` */
  def disallowRootCapability(handler: () => Context ?=> Unit)(using Context): this.type =
    if isUnboxable then handler()
    this

  /** Invoke handler on the elements to ensure wellformedness of the capture set.
   *  The handler might add additional elements to the capture set.
   */
  def ensureWellformed(handler: CaptureRef => Context ?=> Unit)(using Context): this.type =
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
  protected def propagateSolved()(using Context): Unit = ()

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

  /** Apply function `f` to the elements. Typcially used for printing.
   *  Overridden in HiddenSet so that we don't run into infinite recursions
   */
  def processElems[T](f: Refs => T): T = f(elems)

object CaptureSet:
  type Refs = SimpleIdentitySet[CaptureRef]
  type Vars = SimpleIdentitySet[Var]
  type Deps = SimpleIdentitySet[CaptureSet]

  @sharable private var varId = 0

  /** If set to `true`, capture stack traces that tell us where sets are created */
  private final val debugSets = false

  val emptySet = SimpleIdentitySet.empty

  /** The empty capture set `{}` */
  val empty: CaptureSet.Const = Const(emptySet)

  /** The universal capture set `{cap}` */
  def universal(using Context): CaptureSet =
    defn.captureRoot.termRef.singletonCaptureSet

  def fresh(owner: Symbol = NoSymbol)(using Context): CaptureSet =
    Fresh.Cap(owner).singletonCaptureSet

  /** The shared capture set `{cap.rd}` */
  def shared(using Context): CaptureSet =
    defn.captureRoot.termRef.readOnly.singletonCaptureSet

  /** Used as a recursion brake */
  @sharable private[dotc] val Pending = Const(SimpleIdentitySet.empty)

  def apply(elems: CaptureRef*)(using Context): CaptureSet.Const =
    if elems.isEmpty then empty
    else Const(SimpleIdentitySet(elems.map(_.ensuring(_.isTrackableRef))*))

  def apply(elems: Refs)(using Context): CaptureSet.Const =
    if elems.isEmpty then empty else Const(elems)

  /** The subclass of constant capture sets with given elements `elems` */
  class Const private[CaptureSet] (val elems: Refs, val description: String = "") extends CaptureSet:
    def isConst = true
    def isAlwaysEmpty = elems.isEmpty

    def addThisElem(elem: CaptureRef)(using Context, VarState): CompareResult =
      addHiddenElem(elem)

    def addDependent(cs: CaptureSet)(using Context, VarState) = CompareResult.OK

    def upperApprox(origin: CaptureSet)(using Context): CaptureSet = this

    def withDescription(description: String): Const = Const(elems, description)

    def level = undefinedLevel

    def owner = NoSymbol

    override def toString = elems.toString
  end Const

  case class EmptyWithProvenance(ref: CaptureRef, mapped: Type) extends Const(SimpleIdentitySet.empty):
    override def optionalInfo(using Context): String =
      if ctx.settings.YccDebug.value
      then i" under-approximating the result of mapping $ref to $mapped"
      else ""

  /** A special capture set that gets added to the types of symbols that were not
   *  themselves capture checked, in order to admit arbitrary corresponding capture
   *  sets in subcapturing comparisons. Similar to platform types for explicit
   *  nulls, this provides more lenient checking against compilation units that
   *  were not yet compiled with capture checking on.
   */
  object Fluid extends Const(emptySet):
    override def isAlwaysEmpty = false
    override def addThisElem(elem: CaptureRef)(using Context, VarState) = CompareResult.OK
    override def accountsFor(x: CaptureRef)(using Context, VarState): Boolean = true
    override def mightAccountFor(x: CaptureRef)(using Context): Boolean = true
    override def toString = "<fluid>"
  end Fluid

  /** The subclass of captureset variables with given initial elements */
  class Var(override val owner: Symbol = NoSymbol, initialElems: Refs = emptySet, val level: Level = undefinedLevel, underBox: Boolean = false)(using @constructorOnly ictx: Context) extends CaptureSet:

    /** A unique identification number for diagnostics */
    val id =
      varId += 1
      varId

    //assert(id != 40)

    /** A variable is solved if it is aproximated to a from-then-on constant set. */
    private var isSolved: Boolean = false

    /** The elements currently known to be in the set */
    var elems: Refs = initialElems

    /** The sets currently known to be dependent sets (i.e. new additions to this set
     *  are propagated to these dependent sets.)
     */
    var deps: Deps = emptySet

    def isConst = isSolved
    def isAlwaysEmpty = isSolved && elems.isEmpty

    def isMaybeSet = false // overridden in BiMapped

    /** A handler to be invoked if the root reference `cap` is added to this set */
    var rootAddedHandler: () => Context ?=> Unit = () => ()

    private[CaptureSet] var noUniversal = false

    /** A handler to be invoked when new elems are added to this set */
    var newElemAddedHandler: CaptureRef => Context ?=> Unit = _ => ()

    var description: String = ""

    /** Record current elements in given VarState provided it does not yet
     *  contain an entry for this variable.
     */
    private def recordElemsState()(using VarState): Boolean =
      varState.getElems(this) match
        case None => varState.putElems(this, elems)
        case _ => true

    /** Record current dependent sets in given VarState provided it does not yet
     *  contain an entry for this variable.
     */
    private[CaptureSet] def recordDepsState()(using VarState): Boolean =
      varState.getDeps(this) match
        case None => varState.putDeps(this, deps)
        case _ => true

    /** Reset elements to what was recorded in `state` */
    def resetElems()(using state: VarState): Unit =
      elems = state.elems(this)

    /** Reset dependent sets to what was recorded in `state` */
    def resetDeps()(using state: VarState): Unit =
      deps = state.deps(this)

    final def addThisElem(elem: CaptureRef)(using Context, VarState): CompareResult =
      if isConst || !recordElemsState() then // Fail if variable is solved or given VarState is frozen
        addHiddenElem(elem)
      else if Existential.isBadExistential(elem) then // Fail if `elem` is an out-of-scope existential
        CompareResult.Fail(this :: Nil)
      else if !levelOK(elem) then
        CompareResult.LevelError(this, elem)    // or `elem` is not visible at the level of the set.
      else
        // id == 108 then assert(false, i"trying to add $elem to $this")
        assert(elem.isTrackableRef, elem)
        assert(!this.isInstanceOf[HiddenSet] || summon[VarState] == VarState.Separate, summon[VarState])
        elems += elem
        if elem.isRootCapability then
          rootAddedHandler()
        newElemAddedHandler(elem)
        val normElem = if isMaybeSet then elem else elem.stripMaybe
        // assert(id != 5 || elems.size != 3, this)
        val res = (CompareResult.OK /: deps): (r, dep) =>
          r.andAlso(dep.tryInclude(normElem, this))
        res.orElse:
          elems -= elem
          res.addToTrace(this)

    private def levelOK(elem: CaptureRef)(using Context): Boolean =
      if elem.isRootCapability then
        !noUniversal
      else if Existential.isExistentialVar(elem) then
        !noUniversal
        && !TypeComparer.isOpenedExistential(elem)
          // Opened existentials on the left cannot be added to nested capture sets on the right
          // of a comparison. Test case is open-existential.scala.
      else elem match
        case elem: TermRef if level.isDefined =>
          elem.prefix match
            case prefix: CaptureRef =>
              levelOK(prefix)
            case _ =>
              elem.symbol.ccLevel <= level
        case elem: ThisType if level.isDefined =>
          elem.cls.ccLevel.nextInner <= level
        case ReachCapability(elem1) =>
          levelOK(elem1)
        case ReadOnlyCapability(elem1) =>
          levelOK(elem1)
        case MaybeCapability(elem1) =>
          levelOK(elem1)
        case _ =>
          true

    def addDependent(cs: CaptureSet)(using Context, VarState): CompareResult =
      if (cs eq this) || cs.isUniversal || isConst then
        CompareResult.OK
      else if recordDepsState() then
        deps += cs
        CompareResult.OK
      else
        CompareResult.Fail(this :: Nil)

    override def disallowRootCapability(handler: () => Context ?=> Unit)(using Context): this.type =
      noUniversal = true
      rootAddedHandler = handler
      super.disallowRootCapability(handler)

    override def ensureWellformed(handler: CaptureRef => (Context) ?=> Unit)(using Context): this.type =
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
          if approx.elems.exists(Existential.isExistentialVar(_)) then
            ccState.approxWarnings +=
                em"""Capture set variable $this gets upper-approximated
                    |to existential variable from $approx, using {cap} instead."""
            universal
          else approx
        finally computingApprox = false

    /** The intersection of all upper approximations of dependent sets */
    protected def computeApprox(origin: CaptureSet)(using Context): CaptureSet =
      (universal /: deps) { (acc, sup) => acc ** sup.upperApprox(this) }

    /** Widen the variable's elements to its upper approximation and
     *  mark it as constant from now on. This is used for contra-variant type variables
     *  in the results of defs and vals.
     */
    def solve()(using Context): Unit =
      if !isConst then
        val approx = upperApprox(empty).map(Fresh.FromCap(NoSymbol).inverse)
          .showing(i"solve $this = $result", capt)
        //println(i"solving var $this $approx ${approx.isConst} deps = ${deps.toList}")
        val newElems = approx.elems -- elems
        given VarState()
        if tryInclude(newElems, empty).isOK then
          markSolved()

    /** Mark set as solved and propagate this info to all dependent sets */
    def markSolved()(using Context): Unit =
      isSolved = true
      deps.foreach(_.propagateSolved())

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
      val trail = this.match
        case dv: DerivedVar => dv.source.ids
        case _ => ""
      val descr = getClass.getSimpleName.nn.take(1)
      s"$id$descr$trail"
    override def toString = s"Var$id$elems"
  end Var

  /** Variables that represent refinements of class parameters can have the universal
   *  capture set, since they represent only what is the result of the constructor.
   *  Test case: Without that tweak, logger.scala would not compile.
   */
  class RefiningVar(owner: Symbol)(using Context) extends Var(owner):
    override def disallowRootCapability(handler: () => Context ?=> Unit)(using Context) = this

  /** A variable that is derived from some other variable via a map or filter. */
  abstract class DerivedVar(owner: Symbol, initialElems: Refs)(using @constructorOnly ctx: Context)
  extends Var(owner, initialElems):

    // For debugging: A trace where a set was created. Note that logically it would make more
    // sense to place this variable in Mapped, but that runs afoul of the initializatuon checker.
    val stack = if debugSets && this.isInstanceOf[Mapped] then (new Throwable).getStackTrace().nn.take(20) else null

    /** The variable from which this variable is derived */
    def source: Var

    addAsDependentTo(source)

    override def propagateSolved()(using Context) =
      if source.isConst && !isConst then markSolved()
  end DerivedVar

  /** A variable that changes when `source` changes, where all additional new elements are mapped
   *  using   ∪ { tm(x) | x <- source.elems }.
   *  @param source   the original set that is mapped
   *  @param tm       the type map, which is assumed to be idempotent on capture refs
   *                  (except if ccUnsoundMaps is enabled)
   *  @param variance the assumed variance with which types with capturesets of size >= 2 are approximated
   *                  (i.e. co: full capture set, contra: empty set, nonvariant is not allowed.)
   *  @param initial  The initial mappings of source's elements at the point the Mapped set is created.
   */
  class Mapped private[CaptureSet]
    (val source: Var, tm: TypeMap, variance: Int, initial: CaptureSet)(using @constructorOnly ctx: Context)
  extends DerivedVar(source.owner, initial.elems):
    addAsDependentTo(initial)  // initial mappings could change by propagation

    private def mapIsIdempotent = tm.isInstanceOf[IdempotentCaptRefMap]

    assert(ccConfig.allowUnsoundMaps || mapIsIdempotent, tm.getClass)

    private def whereCreated(using Context): String =
      if stack == null then ""
      else i"""
              |Stack trace of variable creation:"
              |${stack.mkString("\n")}"""

    override def tryInclude(elem: CaptureRef, origin: CaptureSet)(using Context, VarState): CompareResult =
      def propagate: CompareResult =
        if (origin ne source) && (origin ne initial) && mapIsIdempotent then
          // `tm` is idempotent, propagate back elems from image set.
          // This is sound, since we know that for `r in newElems: tm(r) = r`, hence
          // `r` is _one_ possible solution in `source` that would make an `r` appear in this set.
          // It's not necessarily the only possible solution, so the scheme is incomplete.
          source.tryInclude(elem, this)
        else if ccConfig.allowUnsoundMaps && !mapIsIdempotent
            && variance <= 0 && !origin.isConst && (origin ne initial) && (origin ne source)
        then
          // The map is neither a BiTypeMap nor an idempotent type map.
          // In that case there's no much we can do.
          // The scheme then does not propagate added elements back to source and rejects adding
          // elements from variable sources in contra- and non-variant positions. In essence,
          // we approximate types resulting from such maps by returning a possible super type
          // from the actual type. But this is neither sound nor complete.
          report.warning(em"trying to add $elem from unrecognized source $origin of mapped set $this$whereCreated")
          CompareResult.Fail(this :: Nil)
        else
          CompareResult.OK
      def propagateIf(cond: Boolean): CompareResult =
        if cond then propagate else CompareResult.OK

      val mapped = extrapolateCaptureRef(elem, tm, variance)

      def isFixpoint =
        mapped.isConst && mapped.elems.size == 1 && mapped.elems.contains(elem)

      def failNoFixpoint =
        val reason =
          if variance <= 0 then i"the set's variance is $variance"
          else i"$elem gets mapped to $mapped, which is not a supercapture."
        report.warning(em"""trying to add $elem from unrecognized source $origin of mapped set $this$whereCreated
                            |The reference cannot be added since $reason""")
        CompareResult.Fail(this :: Nil)

      if origin eq source then // elements have to be mapped
        val added = mapped.elems.filter(!accountsFor(_))
        addNewElems(added)
          .andAlso:
            if mapped.isConst then CompareResult.OK
            else if mapped.asVar.recordDepsState() then { addAsDependentTo(mapped); CompareResult.OK }
            else CompareResult.Fail(this :: Nil)
          .andAlso:
            propagateIf(!added.isEmpty)
      else if accountsFor(elem) then
        CompareResult.OK
      else if variance > 0 then
        // we can soundly add nothing to source and `x` to this set
        addNewElem(elem)
      else if isFixpoint then
        // We can soundly add `x` to both this set and source since `f(x) = x`
        addNewElem(elem).andAlso(propagate)
      else
        // we are out of options; fail (which is always sound).
        failNoFixpoint
    end tryInclude

    override def computeApprox(origin: CaptureSet)(using Context): CaptureSet =
      if source eq origin then
        // it's a mapping of origin, so not a superset of `origin`,
        // therefore don't contribute to the intersection.
        universal
      else
        source.upperApprox(this).map(tm)

    override def propagateSolved()(using Context) =
      if initial.isConst then super.propagateSolved()

    override def toString = s"Mapped$id($source, elems = $elems)"
  end Mapped

  /** A mapping where the type map is required to be a bijection.
   *  Parameters as in Mapped.
   */
  final class BiMapped private[CaptureSet]
    (val source: Var, bimap: BiTypeMap, initialElems: Refs)(using @constructorOnly ctx: Context)
  extends DerivedVar(source.owner, initialElems):

    override def tryInclude(elem: CaptureRef, origin: CaptureSet)(using Context, VarState): CompareResult =
      if origin eq source then
        val mappedElem = bimap.forward(elem)
        if accountsFor(mappedElem) then CompareResult.OK
        else addNewElem(mappedElem)
      else if accountsFor(elem) then
        CompareResult.OK
      else
        source.tryInclude(bimap.backward(elem), this)
          .showing(i"propagating new elem $elem backward from $this to $source = $result", captDebug)
          .andAlso(addNewElem(elem))

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
  end BiMapped

  /** A variable with elements given at any time as { x <- source.elems | p(x) } */
  class Filtered private[CaptureSet]
    (val source: Var, p: Context ?=> CaptureRef => Boolean)(using @constructorOnly ctx: Context)
  extends DerivedVar(source.owner, source.elems.filter(p)):

    override def tryInclude(elem: CaptureRef, origin: CaptureSet)(using Context, VarState): CompareResult =
      if accountsFor(elem) then
        CompareResult.OK
      else if origin eq source then
        if p(elem) then addNewElem(elem)
        else CompareResult.OK
      else
        // Filtered elements have to be back-propagated to source.
        // Elements that don't satisfy `p` are not allowed.
        if p(elem) then source.tryInclude(elem, this)
        else CompareResult.Fail(this :: Nil)

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

    override def tryInclude(elem: CaptureRef, origin: CaptureSet)(using Context, VarState): CompareResult =
      if accountsFor(elem) then CompareResult.OK
      else
        val res = super.tryInclude(elem, origin)
        // If this is the union of a constant and a variable,
        // propagate `elem` to the variable part to avoid slack
        // between the operands and the union.
        if res.isOK && (origin ne cs1) && (origin ne cs2) then
          if cs1.isConst then cs2.tryInclude(elem, origin)
          else if cs2.isConst then cs1.tryInclude(elem, origin)
          else res
        else res

    override def propagateSolved()(using Context) =
      if cs1.isConst && cs2.isConst && !isConst then markSolved()
  end Union

  class Intersection(cs1: CaptureSet, cs2: CaptureSet)(using Context)
  extends Var(initialElems = elemIntersection(cs1, cs2)):
    addAsDependentTo(cs1)
    addAsDependentTo(cs2)
    deps += cs1
    deps += cs2

    override def tryInclude(elem: CaptureRef, origin: CaptureSet)(using Context, VarState): CompareResult =
      val present =
        if origin eq cs1 then cs2.accountsFor(elem)
        else if origin eq cs2 then cs1.accountsFor(elem)
        else true
      if present && !accountsFor(elem) then addNewElem(elem)
      else CompareResult.OK

    override def computeApprox(origin: CaptureSet)(using Context): CaptureSet =
      if (origin eq cs1) || (origin eq cs2) then
        // it's a combination of origin with some other set, so not a superset of `origin`,
        // therefore don't contribute to the intersection.
        universal
      else
        CaptureSet(elemIntersection(cs1.upperApprox(this), cs2.upperApprox(this)))

    override def propagateSolved()(using Context) =
      if cs1.isConst && cs2.isConst && !isConst then markSolved()
  end Intersection

  def elemIntersection(cs1: CaptureSet, cs2: CaptureSet)(using Context): Refs =
    cs1.elems.filter(cs2.mightAccountFor) ++ cs2.elems.filter(cs1.mightAccountFor)

  /** A capture set variable used to record the references hidden by a Fresh.Cap instance */
  class HiddenSet(initialHidden: Refs = emptySet)(using @constructorOnly ictx: Context)
  extends Var(initialElems = initialHidden):

    /** Apply function `f` to `elems` while setting `elems` to empty for the
     *  duration. This is used to escape infinite recursions if two Frash.Caps
     *  refer to each other in their hidden sets.
     */
    override def processElems[T](f: Refs => T): T =
      val savedElems = elems
      elems = emptySet
      try f(savedElems)
      finally elems = savedElems
  end HiddenSet

  /** Extrapolate tm(r) according to `variance`. Let r1 be the result of tm(r).
   *    - If r1 is a tracked CaptureRef, return {r1}
   *    - If r1 has an empty capture set, return {}
   *    - Otherwise,
   *        - if the variance is covariant, return r1's capture set
   *        - if the variance is contravariant, return {}
   *        - Otherwise assertion failure
   */
  def extrapolateCaptureRef(r: CaptureRef, tm: TypeMap, variance: Int)(using Context): CaptureSet =
    val r1 = tm(r)
    val upper = r1.captureSet
    def isExact =
      upper.isAlwaysEmpty
      || upper.isConst && upper.elems.size == 1 && upper.elems.contains(r1)
      || r.derivesFrom(defn.Caps_CapSet)
    if variance > 0 || isExact then upper
    else if variance < 0 then CaptureSet.EmptyWithProvenance(r, r1)
    else upper.maybe

  /** Apply `f` to each element in `xs`, and join result sets with `++` */
  def mapRefs(xs: Refs, f: CaptureRef => CaptureSet)(using Context): CaptureSet =
    ((empty: CaptureSet) /: xs)((cs, x) => cs ++ f(x))

  /** Apply extrapolated `tm` to each element in `xs`, and join result sets with `++` */
  def mapRefs(xs: Refs, tm: TypeMap, variance: Int)(using Context): CaptureSet =
    mapRefs(xs, extrapolateCaptureRef(_, tm, variance))

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
      hiRefs.subCaptures(cs2).isOK && cs2.subCaptures(loRefs).isOK
    case _ =>
      false

  /** A TypeMap with the property that every capture reference in the image
   *  of the map is mapped to itself. I.e. for all capture references r1, r2,
   *  if M(r1) == r2 then M(r2) == r2.
   */
  trait IdempotentCaptRefMap extends TypeMap

  /** A TypeMap that is the identity on capture references */
  trait IdentityCaptRefMap extends TypeMap

  enum CompareResult extends Showable:
    case OK
    case Fail(trace: List[CaptureSet])
    case LevelError(cs: CaptureSet, elem: CaptureRef)

    override def toText(printer: Printer): Text =
      inContext(printer.printerContext):
        this match
          case OK => Str("OK")
          case Fail(trace) =>
            if ctx.settings.YccDebug.value then printer.toText(trace, ", ")
            else blocking.show
          case LevelError(cs: CaptureSet, elem: CaptureRef) =>
            Str(i"($elem at wrong level for $cs at level ${cs.level.toString})")

    /** The result is OK */
    def isOK: Boolean = this == OK

    /** If not isOK, the blocking capture set */
    def blocking: CaptureSet = (this: @unchecked) match
      case Fail(cs) => cs.last
      case LevelError(cs, _) => cs

    /** Optionally, this result if it is a level error */
    def levelError: Option[LevelError] = this match
      case result: LevelError => Some(result)
      case _ => None

    inline def andAlso(op: Context ?=> CompareResult)(using Context): CompareResult =
      if isOK then op else this

    inline def orElse(op: Context ?=> CompareResult)(using Context): CompareResult =
      if isOK then this
      else
        val alt = op
        if alt.isOK then alt
        else this

    inline def addToTrace(cs: CaptureSet): CompareResult = this match
      case Fail(trace) => Fail(cs :: trace)
      case _ => this
  end CompareResult

  /** A VarState serves as a snapshot mechanism that can undo
   *  additions of elements or super sets if an operation fails
   */
  class VarState:

    /** A map from captureset variables to their elements at the time of the snapshot. */
    private val elemsMap: util.EqHashMap[Var, Refs] = new util.EqHashMap

    /** A map from captureset variables to their dependent sets at the time of the snapshot. */
    private val depsMap: util.EqHashMap[Var, Deps] = new util.EqHashMap

    /** The recorded elements of `v` (it's required that a recording was made) */
    def elems(v: Var): Refs = elemsMap(v)

    /** Optionally the recorded elements of `v`, None if nothing was recorded for `v` */
    def getElems(v: Var): Option[Refs] = elemsMap.get(v)

    /** Record elements, return whether this was allowed.
     *  By default, recording is allowed in regular but not in frozen states.
     */
    def putElems(v: Var, elems: Refs): Boolean = { elemsMap(v) = elems; true }

    /** The recorded dependent sets of `v` (it's required that a recording was made) */
    def deps(v: Var): Deps = depsMap(v)

    /** Optionally the recorded dependent sets of `v`, None if nothing was recorded for `v` */
    def getDeps(v: Var): Option[Deps] = depsMap.get(v)

    /** Record dependent sets, return whether this was allowed.
     *  By default, recording is allowed in regular but not in frozen states.
     */
    def putDeps(v: Var, deps: Deps): Boolean = { depsMap(v) = deps; true }

    /** Does this state allow additions of elements to capture set variables? */
    def isOpen = true

    /** Add element to hidden set, recording it in elemsMap,
     *  return whether this was allowed. By default, recording is allowed
     *  but the special state VarState.Separate overrides this.
     */
    def addHidden(hidden: HiddenSet, elem: CaptureRef): Boolean =
      elemsMap.get(hidden) match
        case None => elemsMap(hidden) = hidden.elems
        case _ =>
      hidden.elems += elem
      true

    /** Roll back global state to what was recorded in this VarState */
    def rollBack(): Unit =
      elemsMap.keysIterator.foreach(_.resetElems()(using this))
      depsMap.keysIterator.foreach(_.resetDeps()(using this))

    private var seen: util.EqHashSet[CaptureRef] = new util.EqHashSet

    /** Run test `pred` unless `ref` was seen in an enclosing `ifNotSeen` operation */
    def ifNotSeen(ref: CaptureRef)(pred: => Boolean): Boolean =
      if seen.add(ref) then
        try pred finally seen -= ref
      else false

  object VarState:

    /** A class for states that do not allow to record elements or dependent sets.
     *  In effect this means that no new elements or dependent sets can be added
     *  in these states (since the previous state cannot be recorded in a snapshot)
     *  On the other hand, these states do allow by default Fresh.Cap instances to
     *  subsume arbitary types, which are then recorded in their hidden sets.
     */
    class Closed extends VarState:
      override def putElems(v: Var, refs: Refs) = false
      override def putDeps(v: Var, deps: Deps) = false
      override def isOpen = false

    /** A closed state that allows a Fresh.Cap instance to subsume a
     *  reference `r` only if `r` is already present in the hidden set of the instance.
     *  No new references can be added.
     */
    @sharable
    object Separate extends Closed:
      override def addHidden(hidden: HiddenSet, elem: CaptureRef): Boolean = false

    /** A special state that turns off recording of elements. Used only
     *  in `addSub` to prevent cycles in recordings.
     */
    @sharable
    private[CaptureSet] object Unrecorded extends VarState:
      override def putElems(v: Var, refs: Refs) = true
      override def putDeps(v: Var, deps: Deps) = true
      override def rollBack(): Unit = ()
      override def addHidden(hidden: HiddenSet, elem: CaptureRef): Boolean = true

    /** A closed state that turns off recording of hidden elements (but allows
     *  adding them). Used in `mightAccountFor`.
     */
    @sharable
    private[CaptureSet] object ClosedUnrecorded extends Closed:
      override def addHidden(hidden: HiddenSet, elem: CaptureRef): Boolean = true

  end VarState

  @sharable
  /** The current VarState, as passed by the implicit context */
  def varState(using state: VarState): VarState = state

  /** A template for maps on capabilities where f(c) <: c and f(f(c)) = c */
  private abstract class NarrowingCapabilityMap(using Context) extends BiTypeMap:
    def mapRef(ref: CaptureRef): CaptureRef

    def apply(t: Type) = t match
      case t: CaptureRef if t.isTrackableRef => mapRef(t)
      case _ => mapOver(t)

    lazy val inverse = new BiTypeMap:
      def apply(t: Type) = t // since f(c) <: c, this is the best inverse
      def inverse = NarrowingCapabilityMap.this
      override def toString = NarrowingCapabilityMap.this.toString ++ ".inverse"
  end NarrowingCapabilityMap

  /** Maps `x` to `x?` */
  private class MaybeMap(using Context) extends NarrowingCapabilityMap:
    def mapRef(ref: CaptureRef): CaptureRef = ref.maybe
    override def toString = "Maybe"

  /** Maps `x` to `x.rd` */
  private class ReadOnlyMap(using Context) extends NarrowingCapabilityMap:
    def mapRef(ref: CaptureRef): CaptureRef = ref.readOnly
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

  /** The capture set of the type underlying CaptureRef */
  def ofInfo(ref: CaptureRef)(using Context): CaptureSet = ref match
    case ReachCapability(ref1) =>
      ref1.widen.deepCaptureSet(includeTypevars = true)
        .showing(i"Deep capture set of $ref: ${ref1.widen} = ${result}", capt)
    case ReadOnlyCapability(ref1) =>
      ref1.captureSetOfInfo.map(ReadOnlyMap())
    case _ =>
      if ref.isMaxCapability then ref.singletonCaptureSet
      else ofType(ref.underlying, followResult = true)

  /** Capture set of a type */
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
        case tp @ AnnotatedType(parent, ann) if ann.hasSymbol(defn.ReachCapabilityAnnot) =>
          // Note: we don't use the `ReachCapability(parent)` extractor here since that
          // only works if `parent` is a CaptureRef, but in illegal programs it might not be.
          // And then we do not want to fall back to empty.
          parent match
            case parent: SingletonCaptureRef if parent.isTrackableRef =>
              tp.singletonCaptureSet
            case _ =>
              CaptureSet.ofTypeDeeply(parent.widen)
        case tpd @ defn.RefinedFunctionOf(rinfo: MethodType) if followResult =>
          ofType(tpd.parent, followResult = false)             // pick up capture set from parent type
          ++ (recur(rinfo.resType)                             // add capture set of result
          -- CaptureSet(rinfo.paramRefs.filter(_.isTracked)*)) // but disregard bound parameters
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
   *  NOTE: The traversal logic needs to be in sync with narrowCaps in CaptureOps, which
   *  replaces caps with reach capabilties. The one exception to this is invariant
   *  arguments. This have to be included to be conservative in dcs but must be
   *  excluded in narrowCaps.
   */
  def ofTypeDeeply(tp: Type, includeTypevars: Boolean = false)(using Context): CaptureSet =
    val collect = new TypeAccumulator[CaptureSet]:
      val seen = util.HashSet[Symbol]()
      def apply(cs: CaptureSet, t: Type) =
        if variance < 0 then cs
        else t.dealias match
          case t @ CapturingType(p, cs1) =>
            this(cs, p) ++ cs1
          case t @ AnnotatedType(parent, ann) =>
            this(cs, parent)
          case t: TypeRef if t.symbol.isAbstractOrParamType && !seen.contains(t.symbol) =>
            seen += t.symbol
            val upper = t.info.bounds.hi
            if includeTypevars && upper.isExactlyAny then CaptureSet.fresh(t.symbol)
            else this(cs, upper)
          case t @ FunctionOrMethod(args, res @ Existential(_, _))
          if args.forall(_.isAlwaysPure) =>
            this(cs, Existential.toCap(res))
          case t @ Existential(_, _) =>
            cs
          case _ =>
            foldOver(cs, t)
    collect(CaptureSet.empty, tp)

  type AssumedContains = immutable.Map[TypeRef, SimpleIdentitySet[CaptureRef]]
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

  def levelErrors: Addenda = new Addenda:
    override def toAdd(using Context) =
      for CompareResult.LevelError(cs, ref) <- ccState.levelError.toList yield
        ccState.levelError = None
        if ref.isRootCapability then
          def capStr = if ref.isReadOnly then "cap.rd" else "cap"
          i"""
            |
            |Note that the universal capability `$capStr`
            |cannot be included in capture set $cs"""
        else
          val levelStr = ref match
            case ref: TermRef => i", defined in ${ref.symbol.maybeOwner}"
            case _ => ""
          i"""
            |
            |Note that reference ${ref}$levelStr
            |cannot be included in outer capture set $cs"""

end CaptureSet
