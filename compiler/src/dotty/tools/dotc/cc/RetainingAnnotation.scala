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

  /** Does this annotation refer to a parameter of one of the type lambdas
   *  in `binders`? This is used when transforming inferred types, where
   *  capture sets referring to a type lambda binder enclosing them in the
   *  inferred type itself must be kept: they cannot be re-inferred since
   *  capture set variables would live outside the type lambda's binder,
   *  so level checking would exclude the parameter from them.
   *  The check goes through `retainedElementsRaw` since binder references
   *  can be nested inside further retaining annotations, as in
   *  `retains[CapSet^{C}]`, where `existsPart` alone would not see them.
   *  See issue #26000.
   */
  def refersToParamOf(binders: List[TypeLambda])(using Context): Boolean =
    binders.nonEmpty
    && retainedType.retainedElementsRaw.exists:
        _.existsPart:
          case tp: TypeParamRef => binders.exists(_ eq tp.binder)
          case _ => false
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
