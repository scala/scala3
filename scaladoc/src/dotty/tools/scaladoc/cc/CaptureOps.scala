package dotty.tools.scaladoc

package cc

import scala.quoted._

object CaptureDefs:
  // these should become part of the reflect API in the distant future
  def retains(using qctx: Quotes) = qctx.reflect.Symbol.requiredClass("scala.annotation.retains")
  def retainsCap(using qctx: Quotes) = qctx.reflect.Symbol.requiredClass("scala.annotation.retainsCap")
  def retainsByName(using qctx: Quotes) = qctx.reflect.Symbol.requiredClass("scala.annotation.retainsByName")
  def CapsModule(using qctx: Quotes) = qctx.reflect.Symbol.requiredPackage("scala.caps")
  def captureRoot(using qctx: Quotes) = qctx.reflect.Symbol.requiredPackage("scala.caps.cap")
  def Caps_Capability(using qctx: Quotes) = qctx.reflect.Symbol.requiredClass("scala.caps.Capability")
  def Caps_CapSet(using qctx: Quotes) = qctx.reflect.Symbol.requiredClass("scala.caps.CapSet")
  def Caps_Mutable(using qctx: Quotes) = qctx.reflect.Symbol.requiredClass("scala.caps.Mutable")
  def Caps_SharedCapability(using qctx: Quotes) = qctx.reflect.Symbol.requiredClass("scala.caps.SharedCapability")

  def UseAnnot(using qctx: Quotes) = qctx.reflect.Symbol.requiredClass("scala.caps.use")
  def ConsumeAnnot(using qctx: Quotes) = qctx.reflect.Symbol.requiredClass("scala.caps.consume")

end CaptureDefs

extension (using qctx: Quotes)(ann: qctx.reflect.Symbol)
  /** This symbol is one of `retains` or `retainsCap` */
  def isRetains: Boolean =
    ann == CaptureDefs.retains || ann == CaptureDefs.retainsCap

  /** This symbol is one of `retains`, `retainsCap`, or `retainsByName` */
  def isRetainsLike: Boolean =
    ann.isRetains || ann == CaptureDefs.retainsByName
end extension

extension (using qctx: Quotes)(tpe: qctx.reflect.TypeRepr)
  def isCaptureRoot: Boolean = tpe.termSymbol == CaptureDefs.captureRoot
end extension

/** Decompose capture sets in the union-type-encoding into the sequence of atomic `TypeRepr`s.
 *  Returns `None` if the type is not a capture set.
*/
def decomposeCaptureRefs(using qctx: Quotes)(typ0: qctx.reflect.TypeRepr): Option[List[qctx.reflect.TypeRepr]] =
  import qctx.reflect._
  val buffer = collection.mutable.ListBuffer.empty[TypeRepr]
  def traverse(typ: TypeRepr): Boolean =
    typ match
      case OrType(t1, t2)     => traverse(t1) && traverse(t2)
      case t @ ThisType(_)    => buffer += t; true
      case t @ TermRef(_, _)  => buffer += t; true
      case t @ ParamRef(_, _) => buffer += t; true
       // TODO: are atoms only ever the above? Then we could refine the return type
      case _ => report.warning(s"Unexpected type tree $typ while trying to extract capture references from $typ0"); System.exit(1); false // TODO remove warning eventually
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

def renderCaptureSet(using qctx: Quotes)(refs: List[qctx.reflect.TypeRepr]): List[SignaturePart] =
  import dotty.tools.scaladoc.tasty.NameNormalizer._
  import qctx.reflect._
  refs match
    case List(ref) if ref.isCaptureRoot => List(Keyword("^"))
    case refs =>
      val res0 = refs.map { ref =>
        ref match
          case ThisType(_) => List(Keyword("this"))
          case TermRef(_, sym) => List(Plain(sym)) // FIXME: use type other than Plain, can we have clickable links to say, caps.cap and other things?
          case pf @ ParamRef(tpe, i) => List(Plain(tpe.asInstanceOf[MethodType].paramNames(i))) // FIXME: not sure if this covers all cases
          case _ => List(Plain("<unknown>"))
      }
      val res1 = res0 match
        case Nil => Nil
        case other => other.reduce((r, e) => r ++ (List(Plain(", ")) ++ e))
      Keyword("^") :: Plain("{") :: (res1 ++ List(Plain("}")))