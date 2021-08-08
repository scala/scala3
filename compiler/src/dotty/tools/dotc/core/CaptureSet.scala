package dotty.tools
package dotc
package core

import util.*
import Types.*, Symbols.*, Flags.*, Contexts.*, Decorators.*
import config.Printers.capt
import annotation.threadUnsafe
import annotation.internal.sharable
import reporting.trace
import printing.{Showable, Printer}
import printing.Texts.*

/** A class for capture sets. Capture sets can be constants or variables.
 */
sealed abstract class CaptureSet extends Showable:
  import CaptureSet.*

  /** The elements of this capture set. For capture variables,
   *  the elements known so far.
   */
  def elems: Refs

  /** Is this capture set constant (i.e. not a capture variable)?
   */
  def isConst: Boolean

  /** Is this capture set (always) empty? For capture veraiables, returns
   *  always false
   */
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty

  /** Add new elements to this capture set if allowed.
   *  @pre `newElems` is not empty and does not overlap with `this.elems`.
   *  Constant capture sets never allow to add new elements.
   *  Variables allow it if and only if the new elements can be included
   *  in all their supersets.
   *  @return true iff elements were added
   */
  protected def addNewElems(newElems: Refs)(using Context): Boolean

  /** If this is a variable, add `cs` as a super set */
  protected def addSuper(cs: CaptureSet): this.type

  /** If `cs` is a variable, add this capture set as one of its super sets */
  protected def addSub(cs: CaptureSet): this.type =
    cs.addSuper(this)
    this

  /** Try to include all references of `elems` that are not yet accounted by this
   *  capture set. Inclusion is via `addElems`.
   *  @return true iff elements were added
   */
  protected def tryInclude(elems: Refs)(using Context): Boolean =
    val unaccounted = elems.filter(!accountsFor(_))
    unaccounted.isEmpty || addNewElems(unaccounted)

  /** {x} <:< this   where <:< is subcapturing */
  def accountsFor(x: CaptureRef)(using Context) =
    elems.contains(x) || !x.isRootCapability && x.captureSetOfInfo <:< this

  /** The subcapturing test */
  def <:< (that: CaptureSet)(using Context): Boolean =
    that.tryInclude(elems) && { addSuper(that); true }

  /** The smallest capture set (via <:<) that is a superset of both
   *  `this` and `that`
   */
  def ++ (that: CaptureSet)(using Context): CaptureSet =
    if this.isConst && this.elems.forall(that.accountsFor) then that
    else if that.isConst && that.elems.forall(this.accountsFor) then this
    else if this.isConst && that.isConst then Const(this.elems ++ that.elems)
    else Var(this.elems ++ that.elems).addSub(this).addSub(that)

  /** The smallest superset (via <:<) of this capture set that also contains `ref`.
   */
  def + (ref: CaptureRef)(using Context) = ++ (ref.singletonCaptureSet)

  /** The largest capture set (via <:<) that is a subset of both `this` and `that`
   */
  def intersect(that: CaptureSet)(using Context): CaptureSet =
    if this.isConst && this.elems.forall(that.accountsFor) then this
    else if that.isConst && that.elems.forall(this.accountsFor) then that
    else if this.isConst && that.isConst then Const(this.elems.intersect(that.elems))
    else Var(this.elems.intersect(that.elems)).addSuper(this).addSuper(that)

  /** capture set obtained by applying `f` to all elements of the current capture set
   *  and joining the results. If the current capture set is a variable, the same
   *  transformation is applied to all future additions of new elements.
   */
  def flatMap(f: CaptureRef => CaptureSet)(using Context): CaptureSet =
    mapRefs(elems, f) match
      case cs: Const => cs
      case cs: Var => Mapped(cs, f)

  def substParams(tl: BindingType, to: List[Type])(using Context) =
    flatMap {
      case ref: ParamRef if ref.binder eq tl => to(ref.paramNum).captureSet
      case ref => ref.singletonCaptureSet
    }

  def toRetainsTypeArg(using Context): Type =
    assert(isConst)
    ((NoType: Type) /: elems) ((tp, ref) =>
      if tp.exists then OrType(tp, ref, soft = false) else ref)

  override def toText(printer: Printer): Text =
    Str("{") ~ Text(elems.toList.map(printer.toTextCaptureRef), ", ") ~ Str("}")

object CaptureSet:
  type Refs = SimpleIdentitySet[CaptureRef]
  type Vars = SimpleIdentitySet[Var]
  type Deps = SimpleIdentitySet[CaptureSet]

  private val emptySet = SimpleIdentitySet.empty
  @sharable private var varId = 0

  val empty: CaptureSet = Const(emptySet)

  /** The universal capture set `{*}` */
  def universal(using Context): CaptureSet =
    defn.captureRootType.typeRef.singletonCaptureSet

  /** Used as a recursion brake */
  @sharable private[core] val Pending = Const(SimpleIdentitySet.empty)

  def apply(elems: CaptureRef*)(using Context): CaptureSet =
    if elems.isEmpty then empty
    else Const(SimpleIdentitySet(elems.map(_.normalizedRef)*))

  class Const private[CaptureSet] (val elems: Refs) extends CaptureSet:
    assert(elems != null)
    def isConst = true
    def isEmpty: Boolean = elems.isEmpty

    def addNewElems(elems: Refs)(using Context): Boolean = false
    def addSuper(cs: CaptureSet) = this

    override def toString = elems.toString
  end Const

  class Var private[CaptureSet] (initialElems: Refs) extends CaptureSet:
    val id =
      varId += 1
      varId

    var elems: Refs = initialElems
    var deps: Deps = emptySet
    def isConst = false
    def isEmpty = false

    def addNewElems(newElems: Refs)(using Context): Boolean =
      deps.forall(_.tryInclude(newElems)) && { elems ++= newElems; true }

    def addSuper(cs: CaptureSet) = { deps += cs; this }

    override def toString = s"Var$id$elems"
  end Var

  class Mapped private[CaptureSet] (cv: Var, f: CaptureRef => CaptureSet) extends Var(cv.elems):
    addSub(cv)

    override def accountsFor(x: CaptureRef)(using Context): Boolean =
      f(x).elems.forall(super.accountsFor)

    override def addNewElems(newElems: Refs)(using Context): Boolean =
      super.addNewElems(mapRefs(newElems, f).elems)

    override def toString = s"Mapped$id$elems"
  end Mapped

  def mapRefs(xs: Refs, f: CaptureRef => CaptureSet)(using Context): CaptureSet =
    (empty /: xs)((cs, x) => cs ++ f(x))

  def ofClass(cinfo: ClassInfo, argTypes: List[Type])(using Context): CaptureSet =
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
        parent <- cinfo.parents if parent.classSymbol == defn.RetainsClass
        arg <- parent.argInfos
      yield captureSetOf(arg)
    css.foldLeft(empty)(_ ++ _)

  def ofType(tp: Type)(using Context): CaptureSet =
    def recur(tp: Type): CaptureSet = tp match
      case tp: NamedType =>
        tp.captureSet
      case tp: ParamRef =>
        tp.captureSet
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
        recur(tp1).intersect(recur(tp2))
      case OrType(tp1, tp2) =>
        recur(tp1) ++ recur(tp2)
      case tp: ClassInfo =>
        ofClass(tp, Nil)
      case _ =>
        empty
    recur(tp)
      .showing(i"capture set of $tp = $result", capt)

  def fromRetainsTypeArg(tp: Type)(using Context): CaptureSet = tp match
    case tp: CaptureRef => tp.singletonCaptureSet
    case OrType(tp1, tp2) => fromRetainsTypeArg(tp1) ++ fromRetainsTypeArg(tp2)

end CaptureSet
