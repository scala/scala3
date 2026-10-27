package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*
import Annotations.{Annotation, CompactAnnotation, EmptyAnnotation}
import ast.tpd.TypeTree
import config.Feature

/** A class for annotations @retains, @retainsByName and @retainsCap
 *  We make sure that all annotations with these classes are represented
 *  as RetainingAnnotations.
 */
class RetainingAnnotation(tpe: Type) extends CompactAnnotation(tpe) {

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

  /** Map the retained type with `tm`. If the result is no longer a set of
   *  capabilities, which happens when `tm` approximates, `tm.capturesApproximated`
   *  is set.
   */
  override def mapWith(tm: TypeMap)(using Context): Annotation =
    if Feature.ccEnabledSomewhere then
      val annot1 = mapWithCtd(tm)
      if annot1 ne this then
        annot1 match
          case annot1: RetainingAnnotation
          if !retainsCapabilities(retainedType) || retainsCapabilities(annot1.retainedType) =>
          case _ => tm.capturesApproximated = true
      annot1
    else EmptyAnnotation

  /** Is every element of retained type `tp` a capability or a capture set?
   *  This is false if an element was approximated by a type map, for instance
   *  to a non-singleton type, a skolem, or type bounds, or if a capture set
   *  parameter was replaced by a bare `CapSet`, which is how typer instantiates
   *  a capture set parameter it knows nothing about.
   */
  private def retainsCapabilities(tp: Type)(using Context): Boolean = tp match
    case OrType(tp1, tp2) =>
      retainsCapabilities(tp1) && retainsCapabilities(tp2)
    case AnnotatedType(parent, ann: RetainingAnnotation) =>
      parent.typeSymbol == defn.Caps_CapSet && retainsCapabilities(ann.retainedType)
    case AnnotatedType(parent, _) => // .rd, .only, .except, reach
      isCoreCapability(parent)
    case tp =>
      isCoreCapability(tp) || tp.isRef(defn.NothingClass)

  private def isCoreCapability(tp: Type)(using Context): Boolean = tp match
    case tp: TermRef =>
      !tp.prefix.existsPart(_.isInstanceOf[SkolemType], forceLazy = false)
    case _: (ThisType | TermParamRef | TypeParamRef) =>
      true
    case tp: TypeRef =>
      !tp.symbol.isClass
    case _ =>
      false

  def isStrict(using Context): Boolean = symbol.isRetains

  def retainedType(using Context): Type =
    if symbol == defn.RetainsCapAnnot then defn.Caps_any.termRef
    else argumentType(0)

  private var myCaptureSet: CaptureSet | Null = null

  def toCaptureSet(using Context): CaptureSet =
    if myCaptureSet == null then
      myCaptureSet = CaptureSet(retainedType.retainedElements*)
    myCaptureSet.nn
}
object RetainingAnnotation {

  /** Convert annotation with retains as symbol to a RetainingAnnotation */
  def fromAnnotation(ann: Annotation)(using Context): RetainingAnnotation = ann match
    case ann: RetainingAnnotation => ann
    case _ =>
      assert(ann.symbol.isRetains)
      ann.tree match
        case atree: TypeTree => // this is the case if sourceVersion.enablesCompactAnnotation
          CompactAnnotation(atree.tpe).asInstanceOf[RetainingAnnotation]
        case atree =>
          CompactAnnotation(atree).asInstanceOf[RetainingAnnotation]
}
