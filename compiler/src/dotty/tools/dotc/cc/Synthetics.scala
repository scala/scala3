package dotty.tools
package dotc
package cc

import core.*
import Symbols.*, SymDenotations.*, Contexts.*, Flags.*, Types.*, Decorators.*
import StdNames.nme
import NameKinds.DefaultGetterName

/** Classification and transformation methods for synthetic
 *  case class methods that need to be treated specially.
 *  In particular, compute capturing types for some of these methods which
 *  have inferred (result-)types that need to be established under separate
 *  compilation.
 */
object Synthetics:
  def isSyntheticCopyMethod(sym: SymDenotation)(using Context) =
    sym.name == nme.copy && sym.is(Synthetic) && sym.owner.isClass && sym.owner.is(Case)

  def isSyntheticApplyMethod(sym: SymDenotation)(using Context) =
    sym.name == nme.apply && sym.is(Synthetic) && sym.owner.is(Module) && sym.owner.companionClass.is(Case)

  def isSyntheticUnapplyMethod(sym: SymDenotation)(using Context) =
    sym.name == nme.unapply && sym.is(Synthetic) && sym.owner.is(Module) && sym.owner.companionClass.is(Case)

  def isSyntheticCopyDefaultGetterMethod(sym: SymDenotation)(using Context) = sym.name match
    case DefaultGetterName(nme.copy, _) => sym.is(Synthetic)
    case _ => false

  /** Is `sym` a synthetic apply, copy, or copy default getter method? */
  def needsTransform(sym: SymDenotation)(using Context): Boolean =
    isSyntheticCopyMethod(sym)
    || isSyntheticApplyMethod(sym)
    || isSyntheticUnapplyMethod(sym)
    || isSyntheticCopyDefaultGetterMethod(sym)

  /** Method is excluded from regular capture checking */
  def isExcluded(sym: Symbol)(using Context): Boolean =
    sym.is(Synthetic)
    && sym.owner.isClass
    && ( defn.caseClassSynthesized.exists(
             ccsym => sym.overriddenSymbol(ccsym.owner.asClass) == ccsym)
        || sym.name == nme.fromProduct
        || needsTransform(sym)
      )

  /** Add capture dependencies to the type of `apply` or `copy` method of a case class */
  private def addCaptureDeps(info: Type)(using Context): Type = info match
    case info: MethodType =>
      val trackedParams = info.paramRefs.filter(atPhase(ctx.phase.next)(_.isTracked))
      def augmentResult(tp: Type): Type = tp match
        case tp: MethodOrPoly =>
          tp.derivedLambdaType(resType = augmentResult(tp.resType))
        case _ =>
          val refined = trackedParams.foldLeft(tp) { (parent, pref) =>
            RefinedType(parent, pref.paramName,
              CapturingType(
                atPhase(ctx.phase.next)(pref.underlying.stripCapturing),
                CaptureSet(pref), CapturingKind.Regular))
          }
          CapturingType(refined, CaptureSet(trackedParams*), CapturingKind.Regular)
      if trackedParams.isEmpty then info else augmentResult(info)
    case info: PolyType =>
      info.derivedLambdaType(resType = addCaptureDeps(info.resType))
    case _ =>
      info

  /** Drop capture dependencies from the type of `apply` or `copy` method of a case class */
  private def dropCaptureDeps(tp: Type)(using Context): Type = tp match
    case tp: MethodOrPoly =>
      tp.derivedLambdaType(resType = dropCaptureDeps(tp.resType))
    case CapturingType(parent, _, _) =>
      dropCaptureDeps(parent)
    case RefinedType(parent, _, _) =>
      dropCaptureDeps(parent)
    case _ =>
      tp

  /** Add capture information to the type of the default getter of a case class copy method */
  private def addDefaultGetterCapture(info: Type, owner: Symbol, idx: Int)(using Context): Type = info match
    case info: MethodOrPoly =>
      info.derivedLambdaType(resType = addDefaultGetterCapture(info.resType, owner, idx))
    case info: ExprType =>
      info.derivedExprType(addDefaultGetterCapture(info.resType, owner, idx))
    case EventuallyCapturingType(parent, _, _) =>
      addDefaultGetterCapture(parent, owner, idx)
    case info @ AnnotatedType(parent, annot) =>
      info.derivedAnnotatedType(addDefaultGetterCapture(parent, owner, idx), annot)
    case _ if idx < owner.asClass.paramGetters.length =>
      val param = owner.asClass.paramGetters(idx)
      val pinfo = param.info
      atPhase(ctx.phase.next) {
        if pinfo.captureSet.isAlwaysEmpty then info
        else CapturingType(pinfo.stripCapturing, CaptureSet(param.termRef), CapturingKind.Regular)
      }
    case _ =>
      info

  /** Drop capture information from the type of the default getter of a case class copy method */
  private def dropDefaultGetterCapture(info: Type)(using Context): Type = info match
    case info: MethodOrPoly =>
      info.derivedLambdaType(resType = dropDefaultGetterCapture(info.resType))
    case CapturingType(parent, _, _) =>
      parent
    case info @ AnnotatedType(parent, annot) =>
      info.derivedAnnotatedType(dropDefaultGetterCapture(parent), annot)
    case _ =>
      info

  private def addUnapplyCaptures(info: Type)(using Context): Type = info match
    case info: MethodType =>
      val paramInfo :: Nil = info.paramInfos: @unchecked
      val newParamInfo =
        CapturingType(paramInfo, CaptureSet.universal, CapturingKind.Regular)
      val trackedParam = info.paramRefs.head
      def newResult(tp: Type): Type = tp match
        case tp: MethodOrPoly =>
          tp.derivedLambdaType(resType = newResult(tp.resType))
        case _ =>
          CapturingType(tp, CaptureSet(trackedParam), CapturingKind.Regular)
      info.derivedLambdaType(paramInfos = newParamInfo :: Nil, resType = newResult(info.resType))
    case info: PolyType =>
      info.derivedLambdaType(resType = addUnapplyCaptures(info.resType))

  private def dropUnapplyCaptures(info: Type)(using Context): Type = info match
    case info: MethodType =>
      val CapturingType(oldParamInfo, _, _) :: Nil = info.paramInfos: @unchecked
      def oldResult(tp: Type): Type = tp match
        case tp: MethodOrPoly =>
          tp.derivedLambdaType(resType = oldResult(tp.resType))
        case CapturingType(tp, _, _) =>
          tp
      info.derivedLambdaType(paramInfos = oldParamInfo :: Nil, resType = oldResult(info.resType))
    case info: PolyType =>
      info.derivedLambdaType(resType = dropUnapplyCaptures(info.resType))

  /** If `sym` refers to a synthetic apply, copy, or copy default getter method
   *  of a case class, transform it to account for capture information.
   *  @pre needsTransform(sym)
   */
  def transformToCC(sym: SymDenotation)(using Context): SymDenotation = sym.name match
    case DefaultGetterName(nme.copy, n) if sym.is(Synthetic) && sym.owner.is(Case) =>
      sym.copySymDenotation(info = addDefaultGetterCapture(sym.info, sym.owner, n))
    case nme.unapply =>
      sym.copySymDenotation(info = addUnapplyCaptures(sym.info))
    case _ =>
      sym.copySymDenotation(info = addCaptureDeps(sym.info))

  /** If `sym` refers to a synthetic apply, copy, or copy default getter method
   *  of a case class, transform it back to what it was before the CC phase.
   *  @pre needsTransform(sym)
   */
  def transformFromCC(sym: SymDenotation)(using Context): SymDenotation =
    if isSyntheticCopyDefaultGetterMethod(sym) then
      sym.copySymDenotation(info = dropDefaultGetterCapture(sym.info))
    else if sym.name == nme.unapply then
      sym.copySymDenotation(info = dropUnapplyCaptures(sym.info))
    else
      sym.copySymDenotation(info = dropCaptureDeps(sym.info))

end Synthetics