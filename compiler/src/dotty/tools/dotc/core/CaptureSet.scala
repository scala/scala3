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

case class CaptureSet private (elems: CaptureSet.Refs) extends Showable:
  import CaptureSet.*

  def isEmpty: Boolean = elems.isEmpty
  def nonEmpty: Boolean = !isEmpty

  private var myClosure: Refs | Null = null

  def closure(using Context): Refs =
    if myClosure == null then
      var cl = elems
      var seen: Refs = SimpleIdentitySet.empty
      while
        val prev = cl
        for ref <- cl do
          if !seen.contains(ref) then
            seen += ref
            cl = cl ++ ref.captureSetOfInfo.elems
        prev ne cl
      do ()
      myClosure = cl
    myClosure

  /** {x} <:< this   where <:< is subcapturing */
  def accountsFor(x: CaptureRef)(using Context) =
    elems.contains(x) || !x.isRootCapability && x.captureSetOfInfo <:< this

  /** The subcapturing test */
  def <:< (that: CaptureSet)(using Context): Boolean =
    elems.isEmpty || elems.forall(that.accountsFor)

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

  def ofType(tp: Type)(using Context): CaptureSet =
    val collect = new TypeAccumulator[Refs]:
      var localBinders: SimpleIdentitySet[BindingType] = SimpleIdentitySet.empty
      var seenLazyRefs: SimpleIdentitySet[LazyRef] = SimpleIdentitySet.empty
      def apply(elems: Refs, tp: Type): Refs = trace(i"capt $elems, $tp", show = true) {
        tp match
          case tp: NamedType =>
            if variance < 0 then elems
            else elems ++ tp.captureSet.elems
          case tp: ParamRef =>
            if variance < 0 || localBinders.contains(tp.binder) then elems
            else elems ++ tp.captureSet.elems
          case tp: LambdaType =>
            localBinders += tp
            try apply(elems, tp.resultType)
            finally localBinders -= tp
          case AndType(tp1, tp2) =>
            val elems1 = apply(SimpleIdentitySet.empty, tp1)
            val elems2 = apply(SimpleIdentitySet.empty, tp2)
            elems ++ elems1.intersect(elems2)
          case CapturingType(parent, ref) =>
            val elems1 = apply(elems, parent)
            if variance >= 0 then elems1 + ref else elems1
          case TypeBounds(_, hi) =>
            apply(elems, hi)
          case tp: LazyRef =>
            if seenLazyRefs.contains(tp)
              || tp.evaluating  // shapeless gets an assertion error without this test
            then elems
            else
              seenLazyRefs += tp
              foldOver(elems, tp)
//          case tp: MatchType =>
//            val normed = tp.tryNormalize
//            if normed.exists then apply(elems, normed) else foldOver(elems, tp)
          case _ =>
            foldOver(elems, tp)
      }

    CaptureSet(collect(empty.elems, tp))
      .showing(i"capture set of $tp = $result", capt)

