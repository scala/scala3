package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*
import Annotations.{Annotation, CompactAnnotation, EmptyAnnotation}
import config.Feature

/** A class for annotations @retains, @retainsByName and @retainsCap */
class RetainingAnnotation(tpe: Type) extends CompactAnnotation(tpe):

    /** Sanitize @retains arguments to approximate illegal types that could cause a compilation
     *  time blowup before they are dropped ot detected. This means mapping all all skolems
     *  (?n: T) to (?n: Any), and mapping all recursive captures that are not on CapSet to `^`.
     *  Skolems and capturing types on types other than CapSet are not allowed in a
     *  @retains annotation anyway, so the underlying type does not matter as long as it is also
     *  illegal. See i24556.scala and i24556a.scala.
     */
    override protected def sanitize(tp: Type)(using Context): Type = tp match
      case SkolemType(_) =>
        SkolemType(defn.AnyType)
      case tp @ AnnotatedType(parent, ann)
      if ann.symbol.isRetainsLike && parent.typeSymbol != defn.Caps_CapSet =>
        tp.derivedAnnotatedType(parent, ann.derivedClassAnnotation(defn.RetainsCapAnnot))
      case tp @ OrType(tp1, tp2) =>
        tp.derivedOrType(sanitize(tp1), sanitize(tp2))
      case _ =>
        tp

    override def mapWith(tm: TypeMap)(using Context): Annotation =
      if Feature.ccEnabledSomewhere then mapWithCtd(tm) else EmptyAnnotation

/*
    def toCaptureSet(using Context): CaptureSet =
      CaptureSet(argumentType(0).retainedElements*)
*/
end RetainingAnnotation
