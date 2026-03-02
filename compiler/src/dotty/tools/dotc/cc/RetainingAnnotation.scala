package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*
import Annotations.{Annotation, CompactAnnotation, EmptyAnnotation}
import config.Feature

/** A class for annotations @retains, @retainsByName and @retainsCap
 *  We make sure that all annotations with these classes are represented
 *  as RetainingAnnotations.
 */
class RetainingAnnotation(tpe: Type) extends CompactAnnotation(tpe):

  def this(cls: ClassSymbol, args: Type*)(using Context) = this(cls.typeRef.appliedTo(args.toList))

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
    case tp @ AnnotatedType(parent, ann: RetainingAnnotation)
    if parent.typeSymbol != defn.Caps_CapSet && ann.symbol != defn.RetainsCapAnnot =>
      AnnotatedType(parent, RetainingAnnotation(defn.RetainsCapAnnot))
    case tp @ OrType(tp1, tp2) =>
      tp.derivedOrType(sanitize(tp1), sanitize(tp2))
    case _ =>
      tp

  override def mapWith(tm: TypeMap)(using Context): Annotation =
    if Feature.ccEnabledSomewhere then mapWithCtd(tm) else EmptyAnnotation

  def isStrict(using Context): Boolean = symbol.isRetains

  def retainedType(using Context): Type =
    if symbol == defn.RetainsCapAnnot then defn.Caps_any.termRef
    else argumentType(0)

  private var myCaptureSet: CaptureSet | Null = null

  def toCaptureSet(using Context): CaptureSet =
    if myCaptureSet == null then
      myCaptureSet = CaptureSet(retainedType.retainedElements*)
    myCaptureSet.nn

end RetainingAnnotation
