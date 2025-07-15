package dotty.tools.scaladoc

package cc

import scala.quoted._

object CaptureDefs:
  // these should become part of the reflect API in the distant future
  def retains(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.retains")
  def retainsCap(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.retainsCap")
  def retainsByName(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.retainsByName")
  def CapsModule(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredPackage("scala.caps")
  def captureRoot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredPackage("scala.caps.cap")
  def Caps_Capability(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.Capability")
  def Caps_CapSet(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.CapSet")
  def Caps_Mutable(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.Mutable")
  def Caps_SharedCapability(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.SharedCapability")
  def UseAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.use")
  def ConsumeAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.consume")
  def ReachCapabilityAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.internal.reachCapability")
  def RootCapabilityAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.internal.rootCapability")
  def ReadOnlyCapabilityAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.internal.readOnlyCapability")
  def RequiresCapabilityAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.internal.requiresCapability")
end CaptureDefs

extension (using qctx: Quotes)(ann: qctx.reflect.Symbol)
  /** This symbol is one of `retains` or `retainsCap` */
  def isRetains: Boolean =
    ann == CaptureDefs.retains || ann == CaptureDefs.retainsCap

  /** This symbol is one of `retains`, `retainsCap`, or `retainsByName` */
  def isRetainsLike: Boolean =
    ann.isRetains || ann == CaptureDefs.retainsByName

  def isReachCapabilityAnnot: Boolean =
    ann == CaptureDefs.ReachCapabilityAnnot
end extension

extension (using qctx: Quotes)(tpe: qctx.reflect.TypeRepr)
  def isCaptureRoot: Boolean = tpe.termSymbol == CaptureDefs.captureRoot
end extension

object ReachCapability:
  def unapply(using qctx: Quotes)(ty: qctx.reflect.TypeRepr): Option[qctx.reflect.TypeRepr] =
    import qctx.reflect._
    ty match
      case AnnotatedType(base, Apply(Select(New(annot), _), Nil)) if annot.symbol.isReachCapabilityAnnot =>
        Some(base)
      case _ => None
end ReachCapability

/** Decompose capture sets in the union-type-encoding into the sequence of atomic `TypeRepr`s.
 *  Returns `None` if the type is not a capture set.
*/
def decomposeCaptureRefs(using qctx: Quotes)(typ0: qctx.reflect.TypeRepr): Option[List[qctx.reflect.TypeRepr]] =
  import qctx.reflect._
  val buffer = collection.mutable.ListBuffer.empty[TypeRepr]
  def include(t: TypeRepr): Boolean = { buffer += t; true }
  def traverse(typ: TypeRepr): Boolean =
    typ match
      case OrType(t1, t2)         => traverse(t1) && traverse(t2)
      case t @ ThisType(_)        => include(t)
      case t @ TermRef(_, _)      => include(t)
      case t @ ParamRef(_, _)     => include(t)
      case t @ ReachCapability(_) => include(t)
      case t if t.typeSymbol == defn.NothingClass => true
       // TODO: are atoms only ever the above? Then we could refine the return type
      case _ => report.warning(s"Unexpected type tree $typ while trying to extract capture references from $typ0"); false // TODO remove warning eventually
  if traverse(typ0) then Some(buffer.toList) else None
end decomposeCaptureRefs

object CaptureSetType:
  def unapply(using qctx: Quotes)(tt: qctx.reflect.TypeTree): Option[List[qctx.reflect.TypeRepr]] = decomposeCaptureRefs(tt.tpe)
end CaptureSetType

object CapturingType:
  def unapply(using qctx: Quotes)(typ: qctx.reflect.TypeRepr): Option[(qctx.reflect.TypeRepr, List[qctx.reflect.TypeRepr])] =
    import qctx.reflect._
    typ match
      case AnnotatedType(base, Apply(TypeApply(Select(New(annot), _), List(CaptureSetType(refs))), Nil)) if annot.symbol.isRetainsLike =>
        Some((base, refs))
      case AnnotatedType(base, Apply(Select(New(annot), _), Nil)) if annot.symbol == CaptureDefs.retainsCap =>
        Some((base, List(CaptureDefs.captureRoot.termRef)))
      case _ => None
end CapturingType