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

case class CaptureSet(elems: CaptureSet.Refs) extends Showable:
  import CaptureSet.*

  def isEmpty: Boolean = elems.isEmpty
  def nonEmpty: Boolean = !isEmpty

  def ++ (that: CaptureSet): CaptureSet =
    if this.isEmpty then that
    else if that.isEmpty then this
    else CaptureSet(elems ++ that.elems)

  def + (ref: CaptureRef) =
    if elems.contains(ref) then this
    else CaptureSet(elems + ref)

  def -- (that: CaptureSet)(using Context) =
    CaptureSet(elems.filter(!that.accountsFor(_)))

  def intersect (that: CaptureSet): CaptureSet =
    CaptureSet(this.elems.intersect(that.elems))

  /** {x} <:< this   where <:< is subcapturing */
  def accountsFor(x: CaptureRef)(using Context) =
    elems.contains(x) || !x.isRootCapability && x.captureSetOfInfo <:< this

  /** The subcapturing test */
  def <:< (that: CaptureSet)(using Context): Boolean =
    elems.isEmpty || elems.forall(that.accountsFor)

  def flatMap(f: CaptureRef => CaptureSet)(using Context): CaptureSet =
    (empty /: elems)((cs, ref) => cs ++ f(ref))

  def substParams(tl: BindingType, to: List[Type])(using Context) =
    flatMap {
      case ref: ParamRef if ref.binder eq tl => to(ref.paramNum).captureSet
      case ref => ref.singletonCaptureSet
    }

  def toRetainsTypeArg(using Context): Type =
    ((NoType: Type) /: elems) ((tp, ref) =>
      if tp.exists then OrType(tp, ref, soft = false) else ref)

  override def toString = elems.toString

  override def toText(printer: Printer): Text =
    Str("{") ~ Text(elems.toList.map(printer.toTextCaptureRef), ", ") ~ Str("}")

object CaptureSet:
  type Refs = SimpleIdentitySet[CaptureRef]

   @sharable val empty: CaptureSet = CaptureSet(SimpleIdentitySet.empty)

  /** Used as a recursion brake */
  @sharable private[core] val Pending = CaptureSet(SimpleIdentitySet.empty)

  def apply(elems: CaptureRef*)(using Context): CaptureSet =
    if elems.isEmpty then empty
    else CaptureSet(SimpleIdentitySet(elems.map(_.normalizedRef)*))

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
      case tp: CaptureRef =>
        tp.captureSet
      case CapturingType(parent, ref) =>
        recur(parent) + ref
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
    case tp: CaptureRef if tp.isTracked => tp.singletonCaptureSet
    case OrType(tp1, tp2) => fromRetainsTypeArg(tp1) ++ fromRetainsTypeArg(tp2)
    case _ => empty
