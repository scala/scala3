package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Flags.*, Contexts.*, Decorators.*
import config.Printers.capt
import Annotations.Annotation
import annotation.threadUnsafe
import annotation.constructorOnly
import annotation.internal.sharable
import reporting.trace
import printing.{Showable, Printer}
import printing.Texts.*
import util.{SimpleIdentitySet, Property}
import util.common.alwaysTrue
import scala.collection.mutable
import config.Config.ccAllowUnsoundMaps

/** A class for capture sets. Capture sets can be constants or variables.
 *  Capture sets support inclusion constraints <:< where <:< is subcapturing.
 *  They also allow mapping with arbitrary functions from elements to capture sets,
 *  by supporting a monadic flatMap operation. That is, constraints can be
 *  of one of the following forms
 *
 *    cs1 <:< cs2
 *    cs1 = ∪ {f(x) | x ∈ cs2}
 *
 *  where the `f`s are arbitrary functions from capture references to capture sets.
 *  We call the resulting constraint system "monadic set constraints".
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

  /** Is this capture set always empty? For capture veraiables, returns
   *  always false
   */
  def isAlwaysEmpty: Boolean

  /** Is this capture set definitely non-empty? */
  final def isNotEmpty: Boolean = !elems.isEmpty

  /** Cast to Const. @pre: isConst */
  def asConst: Const = this match
    case c: Const => c
    case v: Var =>
      assert(v.isConst)
      Const(v.elems)

  final def isUniversal(using Context) =
    elems.exists {
      case ref: TermRef => ref.symbol == defn.captureRoot
      case _ => false
    }

  /** Cast to variable. @pre: !isConst */
  def asVar: Var =
    assert(!isConst)
    asInstanceOf[Var]

  /** Add new elements to this capture set if allowed.
   *  @pre `newElems` is not empty and does not overlap with `this.elems`.
   *  Constant capture sets never allow to add new elements.
   *  Variables allow it if and only if the new elements can be included
   *  in all their supersets.
   *  @param origin   The set where the elements come from, or `empty` if not known.
   *  @return CompareResult.OK if elements were added, or a conflicting
   *          capture set that prevents addition otherwise.
   */
  protected def addNewElems(newElems: Refs, origin: CaptureSet)(using Context, VarState): CompareResult

  /** If this is a variable, add `cs` as a super set */
  protected def addSuper(cs: CaptureSet)(using Context, VarState): CompareResult

  /** If `cs` is a variable, add this capture set as one of its super sets */
  protected def addSub(cs: CaptureSet)(using Context): this.type =
    cs.addSuper(this)(using ctx, UnrecordedState)
    this

  /** Try to include all references of `elems` that are not yet accounted by this
   *  capture set. Inclusion is via `addNewElems`.
   *  @param origin   The set where the elements come from, or `empty` if not known.
   *  @return  CompareResult.OK if all unaccounted elements could be added,
   *           capture set that prevents addition otherwise.
   */
  protected final def tryInclude(elems: Refs, origin: CaptureSet)(using Context, VarState): CompareResult =
    val unaccounted = elems.filter(!accountsFor(_))
    if unaccounted.isEmpty then CompareResult.OK
    else addNewElems(unaccounted, origin)

  protected final def tryInclude(elem: CaptureRef, origin: CaptureSet)(using Context, VarState): CompareResult =
    if accountsFor(elem) then CompareResult.OK
    else addNewElems(elem.singletonCaptureSet.elems, origin)

  extension (x: CaptureRef) private def subsumes(y: CaptureRef) =
    (x eq y)
    || y.match
        case y: TermRef => y.prefix eq x
        case _ => false

  /** {x} <:< this   where <:< is subcapturing, but treating all variables
   *                 as frozen.
   */
  def accountsFor(x: CaptureRef)(using Context): Boolean =
    reporting.trace(i"$this accountsFor $x, ${x.captureSetOfInfo}?", show = true) {
      elems.exists(_.subsumes(x))
      || !x.isRootCapability && x.captureSetOfInfo.subCaptures(this, frozen = true).isOK
    }

  /** A more optimistic version of accountsFor, which does not take variable supersets
   *  of the `x` reference into account. A set might account for `x` if it accounts
   *  for `x` in a state where we assume all supersets of `x` have just the elements
   *  known at this point.
   */
  def mightAccountFor(x: CaptureRef)(using Context): Boolean =
    reporting.trace(i"$this mightAccountFor $x, ${x.captureSetOfInfo}?", show = true) {
      elems.exists(_.subsumes(x))
      || !x.isRootCapability && x.captureSetOfInfo.elems.forall(mightAccountFor)
    }

  /** A more optimistic version of subCaptures used to choose one of two typing rules
   *  for selctions and applications. `cs1 mightSubcapture cs2` if `cs2` might account for
   *  every element currently known to be in `cs1`.
   */
  def mightSubcapture(that: CaptureSet)(using Context): Boolean =
    elems.forall(that.mightAccountFor)

  /** The subcapturing test */
  final def subCaptures(that: CaptureSet, frozen: Boolean)(using Context): CompareResult =
    subCaptures(that)(using ctx, if frozen then FrozenState else VarState())

  private def subCaptures(that: CaptureSet)(using Context, VarState): CompareResult =
    def recur(elems: List[CaptureRef]): CompareResult = elems match
      case elem :: elems1 =>
        var result = that.tryInclude(elem, this)
        if !result.isOK && !elem.isRootCapability && summon[VarState] != FrozenState then
          result = elem.captureSetOfInfo.subCaptures(that)
        if result.isOK then
          recur(elems1)
        else
          varState.abort()
          result
      case Nil =>
        addSuper(that)
    recur(elems.toList)
      .showing(i"subcaptures $this <:< $that = ${result.show}", capt)

  def =:= (that: CaptureSet)(using Context): Boolean =
       this.subCaptures(that, frozen = true).isOK
    && that.subCaptures(this, frozen = true).isOK

  /** The smallest capture set (via <:<) that is a superset of both
   *  `this` and `that`
   */
  def ++ (that: CaptureSet)(using Context): CaptureSet =
    if this.subCaptures(that, frozen = true).isOK then that
    else if that.subCaptures(this, frozen = true).isOK then this
    else if this.isConst && that.isConst then Const(this.elems ++ that.elems)
    else Var(this.elems ++ that.elems).addSub(this).addSub(that)

  /** The smallest superset (via <:<) of this capture set that also contains `ref`.
   */
  def + (ref: CaptureRef)(using Context): CaptureSet =
    this ++ ref.singletonCaptureSet

  /** The largest capture set (via <:<) that is a subset of both `this` and `that`
   */
  def **(that: CaptureSet)(using Context): CaptureSet =
    if this.subCaptures(that, frozen = true).isOK then this
    else if that.subCaptures(this, frozen = true).isOK then that
    else if this.isConst && that.isConst then Const(elemIntersection(this, that))
    else Intersected(this, that)

  def -- (that: CaptureSet.Const)(using Context): CaptureSet =
    val elems1 = elems.filter(!that.accountsFor(_))
    if elems1.size == elems.size then this
    else if this.isConst then Const(elems1)
    else Diff(asVar, that)

  def - (ref: CaptureRef)(using Context): CaptureSet =
    this -- ref.singletonCaptureSet

  def disallowRootCapability(handler: () => Unit)(using Context): this.type =
    if isUniversal then handler()
    this

  def filter(p: CaptureRef => Boolean)(using Context): CaptureSet =
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
    case tm: IdentityCaptRefMap => this
    case _ =>
      val mapped = mapRefs(elems, tm, tm.variance)
      if isConst then
        if mapped.isConst && mapped.elems == elems then this
        else mapped
      else tm match
        case tm: IdempotentCaptRefMap => Mapped(asVar, tm, tm.variance, mapped)
        case _ if ccAllowUnsoundMaps => OtherMapped(asVar, tm, tm.variance, mapped)

  def substParams(tl: BindingType, to: List[Type])(using Context) =
    map(Substituters.SubstParamsMap(tl, to))

  /** An upper approximation of this capture set. This is the set itself
   *  except for real (non-mapped, non-filtered) capture set variables, where
   *  it is the intersection of all upper approximations of known supersets
   *  of the variable.
   *  The upper approximation is meaningful only if it is constant. If not,
   *  `upperApprox` can return an arbitrary capture set variable.
   */
  protected def upperApprox(origin: CaptureSet)(using Context): CaptureSet

  protected def propagateSolved()(using Context): Unit = ()

  /** This capture set with a description that tells where it comes from */
  def withDescription(description: String): CaptureSet

  /** The provided description (using `withDescription`) for this capture set or else "" */
  def description: String

  def toRetainsTypeArg(using Context): Type =
    assert(isConst)
    ((NoType: Type) /: elems) ((tp, ref) =>
      if tp.exists then OrType(tp, ref, soft = false) else ref)

  def toRegularAnnotation(cls: Symbol)(using Context): Annotation =
    Annotation(CaptureAnnotation(this, boxed = false)(cls).tree)

  override def toText(printer: Printer): Text =
    Str("{") ~ Text(elems.toList.map(printer.toTextCaptureRef), ", ") ~ Str("}") ~~ description

object CaptureSet:
  type Refs = SimpleIdentitySet[CaptureRef]
  type Vars = SimpleIdentitySet[Var]
  type Deps = SimpleIdentitySet[CaptureSet]

  /** If set to `true`, capture stack traces that tell us where sets are created */
  private final val debugSets = false

  private val emptySet = SimpleIdentitySet.empty
  @sharable private var varId = 0

  val empty: CaptureSet.Const = Const(emptySet)

  /** The universal capture set `{*}` */
  def universal(using Context): CaptureSet =
    defn.captureRoot.termRef.singletonCaptureSet

  /** Used as a recursion brake */
  @sharable private[dotc] val Pending = Const(SimpleIdentitySet.empty)

  def apply(elems: CaptureRef*)(using Context): CaptureSet.Const =
    if elems.isEmpty then empty
    else Const(SimpleIdentitySet(elems.map(_.normalizedRef)*))

  def apply(elems: Refs)(using Context): CaptureSet.Const =
    if elems.isEmpty then empty else Const(elems)

  class Const private[CaptureSet] (val elems: Refs, val description: String = "") extends CaptureSet:
    def isConst = true
    def isAlwaysEmpty = elems.isEmpty

    def addNewElems(elems: Refs, origin: CaptureSet)(using Context, VarState): CompareResult =
      CompareResult.fail(this)

    def addSuper(cs: CaptureSet)(using Context, VarState) = CompareResult.OK

    def upperApprox(origin: CaptureSet)(using Context): CaptureSet = this

    def withDescription(description: String): Const = Const(elems, description)

    override def toString = elems.toString
  end Const

  class Var(initialElems: Refs = emptySet) extends CaptureSet:
    val id =
      varId += 1
      varId

    private var isSolved: Boolean = false

    var elems: Refs = initialElems
    var deps: Deps = emptySet
    def isConst = isSolved
    def isAlwaysEmpty = false
    var addRootHandler: () => Unit = () => ()

    var description: String = ""

    private def recordElemsState()(using VarState): Boolean =
      varState.getElems(this) match
        case None => varState.putElems(this, elems)
        case _ => true

    private[CaptureSet] def recordDepsState()(using VarState): Boolean =
      varState.getDeps(this) match
        case None => varState.putDeps(this, deps)
        case _ => true

    def resetElems()(using state: VarState): Unit =
      elems = state.elems(this)

    def resetDeps()(using state: VarState): Unit =
      deps = state.deps(this)

    def addNewElems(newElems: Refs, origin: CaptureSet)(using Context, VarState): CompareResult =
      if !isConst && recordElemsState() then
        elems ++= newElems
        if isUniversal then addRootHandler()
        // assert(id != 2 || elems.size != 2, this)
        (CompareResult.OK /: deps) { (r, dep) =>
          r.andAlso(dep.tryInclude(newElems, this))
        }
      else
        CompareResult.fail(this)

    def addSuper(cs: CaptureSet)(using Context, VarState): CompareResult =
      if (cs eq this) || cs.elems.contains(defn.captureRoot.termRef) || isConst then
        CompareResult.OK
      else if recordDepsState() then
        deps += cs
        CompareResult.OK
      else
        CompareResult.fail(this)

    override def disallowRootCapability(handler: () => Unit)(using Context): this.type =
      addRootHandler = handler
      super.disallowRootCapability(handler)

    private var computingApprox = false

    final def upperApprox(origin: CaptureSet)(using Context): CaptureSet =
      if computingApprox then universal
      else if isConst then this
      else
        computingApprox = true
        try computeApprox(origin).ensuring(_.isConst)
        finally computingApprox = false

    def withDescription(description: String): this.type =
      this.description =
        if this.description.isEmpty then description
        else s"${this.description} and $description"
      this

    protected def computeApprox(origin: CaptureSet)(using Context): CaptureSet =
      (universal /: deps) { (acc, sup) => acc ** sup.upperApprox(this) }

    /** Widen the variable's elements to its upper approximation and
     *  mark it as constant from now on.
     */
    def solve()(using Context): Unit =
      if !isConst then
        val approx = upperApprox(empty)
        //println(i"solving var $this $approx ${approx.isConst} deps = ${deps.toList}")
        val newElems = approx.elems -- elems
        if newElems.isEmpty || addNewElems(newElems, empty)(using ctx, VarState()).isOK then
          markSolved()

    def markSolved()(using Context): Unit =
      isSolved = true
      deps.foreach(_.propagateSolved())

    protected def ids(using Context): String =
      val trail = this.match
        case dv: DerivedVar => dv.source.ids
        case _ => ""
      s"$id${getClass.getSimpleName.nn.take(1)}$trail"

    override def toText(printer: Printer): Text = inContext(printer.printerContext) {
      for vars <- ctx.property(ShownVars) do vars += this
      super.toText(printer) ~ (Str(ids) provided !isConst && ctx.settings.YccDebug.value)
    }

    override def toString = s"Var$id$elems"
  end Var

  abstract class DerivedVar(initialElems: Refs)(using @constructorOnly ctx: Context)
  extends Var(initialElems):
    def source: Var

    addSub(source)

    override def propagateSolved()(using Context) =
      if source.isConst && !isConst then markSolved()
  end DerivedVar

  /** A variable that changes when `source` changes, where all additional new elements are mapped
   *  using   ∪ { f(x) | x <- elems }.
   */
  class Mapped private[CaptureSet]
    (val source: Var, tm: TypeMap, variance: Int, initial: CaptureSet)(using @constructorOnly ctx: Context)
  extends DerivedVar(initial.elems):
    addSub(initial)
    val stack = if debugSets then (new Throwable).getStackTrace().nn.take(20) else null

    override def addNewElems(newElems: Refs, origin: CaptureSet)(using Context, VarState): CompareResult =
      addNewElemsImpl(newElems: Refs, origin: CaptureSet)
        .andAlso(if origin ne source then source.tryInclude(newElems, this) else CompareResult.OK)
          // `tm` is assumed idempotent, propagate back elems from image set.
          // This is sound, since we know that for `r in newElems: tm(r) = r`, hence
          // `r` is _one_ possible solution in `source` that would make an `r` appear in this set.
          // It's not necessarily the only possible solultion, so the scheme is incomplete.

    protected def addNewElemsImpl(newElems: Refs, origin: CaptureSet)(using Context, VarState): CompareResult =
      val added =
        if origin eq source then mapRefs(newElems, tm, variance)
        else Const(newElems)
      super.addNewElems(added.elems, origin)
        .andAlso {
          if added.isConst then CompareResult.OK
          else if added.asVar.recordDepsState() then { addSub(added); CompareResult.OK }
          else CompareResult.fail(this)
        }

    override def computeApprox(origin: CaptureSet)(using Context): CaptureSet =
      if source eq origin then universal
      else source.upperApprox(this).map(tm)

    override def propagateSolved()(using Context) =
      if initial.isConst then super.propagateSolved()

    override def toString = s"Mapped$id($source, elems = $elems)"
  end Mapped


  /** Should be avoided as much as possible:
   *  A mapped set that uses neither a BiTypeMap nor an idempotent type map.
   *  In that case there's no much we can do.
   *  The patch does not propagate added elements back to source and rejects adding
   *  elements from variable sources in contra- and non-variant positions. In essence,
   *  we approximate types resulting from such maps by returning a possible super type
   *  from the actual type.
   */
  final class OtherMapped private[CaptureSet]
    (source: Var, tm: TypeMap, variance: Int, initial: CaptureSet)(using @constructorOnly ctx: Context)
  extends Mapped(source, tm, variance, initial):

    private def whereCreated(using Context): String =
      if stack == null then ""
      else i"""
              |Stack trace of variable creation:"
              |${stack.mkString("\n")}"""

    override def addNewElems(newElems: Refs, origin: CaptureSet)(using Context, VarState): CompareResult =
      if variance <= 0 && !origin.isConst && (origin ne initial) && (origin ne source) then
        report.warning(i"trying to add elems ${CaptureSet(newElems)} from unrecognized source $origin of mapped set $this$whereCreated")
        CompareResult.fail(this)
      else
        addNewElemsImpl(newElems, origin)
  end OtherMapped

  final class BiMapped private[CaptureSet]
    (val source: Var, bimap: BiTypeMap, initialElems: Refs)(using @constructorOnly ctx: Context)
  extends DerivedVar(initialElems):

    override def addNewElems(newElems: Refs, origin: CaptureSet)(using Context, VarState): CompareResult =
      if origin eq source then
        super.addNewElems(newElems.map(bimap.forward), origin)
      else
        super.addNewElems(newElems, origin)
          .andAlso {
            source.tryInclude(newElems.map(bimap.backward), this)
              .showing(i"propagating new elems ${CaptureSet(newElems)} backward from $this to $source", capt)
          }

    override def computeApprox(origin: CaptureSet)(using Context): CaptureSet =
      val supApprox = super.computeApprox(this)
      if source eq origin then supApprox.map(bimap.inverseTypeMap)
      else source.upperApprox(this).map(bimap) ** supApprox

    override def toString = s"BiMapped$id($source, elems = $elems)"
  end BiMapped

  /** A variable with elements given at any time as { x <- source.elems | p(x) } */
  class Filtered private[CaptureSet]
    (val source: Var, p: CaptureRef => Boolean)(using @constructorOnly ctx: Context)
  extends DerivedVar(source.elems.filter(p)):

    override def addNewElems(newElems: Refs, origin: CaptureSet)(using Context, VarState): CompareResult =
      super.addNewElems(newElems.filter(p), origin)

    override def computeApprox(origin: CaptureSet)(using Context): CaptureSet =
      if source eq origin then universal
      else source.upperApprox(this).filter(p)

    override def toString = s"${getClass.getSimpleName}$id($source, elems = $elems)"
  end Filtered

  /** A variable with elements given at any time as { x <- source.elems | !other.accountsFor(x) } */
  class Diff(source: Var, other: Const)(using Context)
  extends Filtered(source, !other.accountsFor(_))

  def elemIntersection(cs1: CaptureSet, cs2: CaptureSet)(using Context): Refs =
    cs1.elems.filter(cs2.mightAccountFor) ++ cs2.elems.filter(cs1.mightAccountFor)

  class Intersected(cs1: CaptureSet, cs2: CaptureSet)(using Context)
  extends Var(elemIntersection(cs1, cs2)):
    addSub(cs1)
    addSub(cs2)
    deps += cs1
    deps += cs2

    override def addNewElems(newElems: Refs, origin: CaptureSet)(using Context, VarState): CompareResult =
      super.addNewElems(
        if origin eq cs1 then newElems.filter(cs2.accountsFor)
        else if origin eq cs2 then newElems.filter(cs1.accountsFor)
        else newElems, origin)

    override def computeApprox(origin: CaptureSet)(using Context): CaptureSet =
      if (origin eq cs1) || (origin eq cs2) then universal
      else CaptureSet(elemIntersection(cs1.upperApprox(this), cs2.upperApprox(this)))

    override def propagateSolved()(using Context) =
      if cs1.isConst && cs2.isConst && !isConst then markSolved()
  end Intersected

  def extrapolateCaptureRef(r: CaptureRef, tm: TypeMap, variance: Int)(using Context): CaptureSet =
    val r1 = tm(r)
    val upper = r1.captureSet
    def isExact =
      upper.isAlwaysEmpty || upper.isConst && upper.elems.size == 1 && upper.elems.contains(r1)
    if variance > 0 || isExact then upper
    else if variance < 0 then CaptureSet.empty
    else assert(false, i"trying to add $upper from $r via ${tm.getClass} in a non-variant setting")

  def mapRefs(xs: Refs, f: CaptureRef => CaptureSet)(using Context): CaptureSet =
    ((empty: CaptureSet) /: xs)((cs, x) => cs ++ f(x))

  def mapRefs(xs: Refs, tm: TypeMap, variance: Int)(using Context): CaptureSet =
    mapRefs(xs, extrapolateCaptureRef(_, tm, variance))

  def subCapturesRange(arg1: TypeBounds, arg2: Type)(using Context): Boolean = arg1 match
    case TypeBounds(CapturingType(lo, loRefs), CapturingType(hi, hiRefs)) if lo =:= hi =>
      given VarState = VarState()
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

  type CompareResult = CompareResult.Type

  /** None = ok, Some(cs) = failure since not a subset of cs */
  object CompareResult:
    opaque type Type = CaptureSet
    val OK: Type = Const(emptySet)
    def fail(cs: CaptureSet): Type = cs
    extension (result: Type)
      def isOK: Boolean = result eq OK
      def blocking: CaptureSet = result
      def show(using Context): String = if result.isOK then "OK" else i"$result"
      inline def andAlso(op: Context ?=> Type)(using Context): Type = if result.isOK then op else result

  class VarState:
    private val elemsMap: util.EqHashMap[Var, Refs] = new util.EqHashMap
    private val depsMap: util.EqHashMap[Var, Deps] = new util.EqHashMap

    def elems(v: Var): Refs = elemsMap(v)
    def getElems(v: Var): Option[Refs] = elemsMap.get(v)
    def putElems(v: Var, elems: Refs): Boolean = { elemsMap(v) = elems; true }

    def deps(v: Var): Deps = depsMap(v)
    def getDeps(v: Var): Option[Deps] = depsMap.get(v)
    def putDeps(v: Var, deps: Deps): Boolean = { depsMap(v) = deps; true }

    def abort(): Unit =
      elemsMap.keysIterator.foreach(_.resetElems()(using this))
      depsMap.keysIterator.foreach(_.resetDeps()(using this))
  end VarState

  @sharable
  object FrozenState extends VarState:
    override def putElems(v: Var, refs: Refs) = false
    override def putDeps(v: Var, deps: Deps) = false
    override def abort(): Unit = ()

  @sharable
  object UnrecordedState extends VarState:
    override def putElems(v: Var, refs: Refs) = true
    override def putDeps(v: Var, deps: Deps) = true
    override def abort(): Unit = ()

  def varState(using state: VarState): VarState = state

  def ofClass(cinfo: ClassInfo, argTypes: List[Type])(using Context): CaptureSet =
    CaptureSet.empty
    /*
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

  def ofInfo(ref: CaptureRef)(using Context): CaptureSet = ref match
    case ref: TermRef if ref.isRootCapability => ref.singletonCaptureSet
    case _ => ofType(ref.underlying)

  def ofType(tp: Type)(using Context): CaptureSet =
    def recur(tp: Type): CaptureSet = tp.dealias match
      case tp: TermRef =>
        tp.captureSet
      case tp: TermParamRef =>
        tp.captureSet
      case _: TypeRef =>
        if tp.classSymbol.hasAnnotation(defn.CapabilityAnnot) then universal else empty
      case _: TypeParamRef =>
        empty
      case CapturingType(parent, refs) =>
        recur(parent) ++ refs
      case AppliedType(tycon, args) =>
        val cs = recur(tycon)
        tycon.typeParams match
          case tparams @ (LambdaParam(tl, _) :: _) => cs.substParams(tl, args)
          case _ => cs
      case tp: TypeProxy =>
        recur(tp.underlying)
      case AndType(tp1, tp2) =>
        recur(tp1) ** recur(tp2)
      case OrType(tp1, tp2) =>
        recur(tp1) ++ recur(tp2)
      case tp: ClassInfo =>
        ofClass(tp, Nil)
      case _ =>
        empty
    recur(tp)
      .showing(i"capture set of $tp = $result", capt)

  private val ShownVars: Property.Key[mutable.Set[Var]] = Property.Key()

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
