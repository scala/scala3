package dotty.tools
package dotc
package cc

import core.*
import Symbols.*, SymDenotations.*, Contexts.*, Flags.*, Types.*, Decorators.*
import StdNames.nme
import Names.Name
import NameKinds.DefaultGetterName
import Phases.checkCapturesPhase
import config.Printers.capt

/** Classification and transformation methods for synthetic
 *  case class methods that need to be treated specially.
 *  In particular, compute capturing types for some of these methods which
 *  have inferred (result-)types that need to be established under separate
 *  compilation.
 */
object Synthetics:
  private def isSyntheticCopyMethod(sym: SymDenotation)(using Context) =
    sym.name == nme.copy && sym.is(Synthetic) && sym.owner.isClass && sym.owner.is(Case)

  private def isSyntheticCompanionMethod(sym: SymDenotation, names: Name*)(using Context): Boolean =
     names.contains(sym.name) && sym.is(Synthetic) && sym.owner.is(Module) && sym.owner.companionClass.is(Case)

  private def isSyntheticCopyDefaultGetterMethod(sym: SymDenotation)(using Context) = sym.name match
    case DefaultGetterName(nme.copy, _) => sym.is(Synthetic) && sym.owner.isClass && sym.owner.is(Case)
    case _ => false

  /** Is `sym` a synthetic apply, copy, or copy default getter method?
   *  The types of these symbols are transformed in a special way without
   *  looking at the definitions's RHS
   */
  def needsTransform(symd: SymDenotation)(using Context): Boolean =
    isSyntheticCopyMethod(symd)
    || isSyntheticCompanionMethod(symd, nme.apply, nme.unapply)
    || isSyntheticCopyDefaultGetterMethod(symd)
    || (symd.symbol eq defn.Object_eq)
    || (symd.symbol eq defn.Object_ne)

  /** Method is excluded from regular capture checking.
   *  Excluded are synthetic class members
   *   - that override a synthesized case class symbol, or
   *   - the fromProduct method, or
   *   - members transformed specially as indicated by `needsTransform`.
   */
  def isExcluded(sym: Symbol)(using Context): Boolean =
    sym.is(Synthetic)
    && sym.owner.isClass
    && ( defn.caseClassSynthesized.exists(
             ccsym => sym.overriddenSymbol(ccsym.owner.asClass) == ccsym)
        || isSyntheticCompanionMethod(sym, nme.fromProduct)
        || needsTransform(sym))

  /** Add capture dependencies to the type of the `apply` or `copy` method of a case class.
   *  An apply method in a case class like this:
   *    case class CC(a: {d} A, b: B, {cap} c: C)
   *  would get type
   *    def apply(a': {d} A, b: B, {cap} c': C): {a', c'} CC { val a = {a'} A, val c = {c'} C }
   *  where `'` is used to indicate the difference between parameter symbol and refinement name.
   *  Analogous for the copy method.
   */
  private def addCaptureDeps(info: Type)(using Context): Type = info match
    case info: MethodType =>
      val trackedParams = info.paramRefs.filter(atPhase(checkCapturesPhase)(_.isTracked))
      def augmentResult(tp: Type): Type = tp match
        case tp: MethodOrPoly =>
          tp.derivedLambdaType(resType = augmentResult(tp.resType))
        case _ =>
          val refined = trackedParams.foldLeft(tp) { (parent, pref) =>
            RefinedType(parent, pref.paramName,
              CapturingType(
                atPhase(ctx.phase.next)(pref.underlying.stripCapturing),
                CaptureSet(pref)))
          }
          CapturingType(refined, CaptureSet(trackedParams*))
      if trackedParams.isEmpty then info
      else augmentResult(info).showing(i"augment apply/copy type $info to $result", capt)
    case info: PolyType =>
      info.derivedLambdaType(resType = addCaptureDeps(info.resType))
    case _ =>
      info

  /** Drop capture dependencies from the type of `apply` or `copy` method of a case class */
  private def dropCaptureDeps(tp: Type)(using Context): Type = tp match
    case tp: MethodOrPoly =>
      tp.derivedLambdaType(resType = dropCaptureDeps(tp.resType))
    case CapturingType(parent, _) =>
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
    case EventuallyCapturingType(parent, _) =>
      addDefaultGetterCapture(parent, owner, idx)
    case info @ AnnotatedType(parent, annot) =>
      info.derivedAnnotatedType(addDefaultGetterCapture(parent, owner, idx), annot)
    case _ if idx < owner.asClass.paramGetters.length =>
      val param = owner.asClass.paramGetters(idx)
      val pinfo = param.info
      atPhase(ctx.phase.next) {
        if pinfo.captureSet.isAlwaysEmpty then info
        else CapturingType(pinfo.stripCapturing, CaptureSet(param.termRef))
      }
    case _ =>
      info

  /** Drop capture information from the type of the default getter of a case class copy method */
  private def dropDefaultGetterCapture(info: Type)(using Context): Type = info match
    case info: MethodOrPoly =>
      info.derivedLambdaType(resType = dropDefaultGetterCapture(info.resType))
    case CapturingType(parent, _) =>
      parent
    case info @ AnnotatedType(parent, annot) =>
      info.derivedAnnotatedType(dropDefaultGetterCapture(parent), annot)
    case _ =>
      info

  /** Augment an unapply of type `(x: C): D` to `(x: {cap} C): {x} D` */
  private def addUnapplyCaptures(info: Type)(using Context): Type = info match
    case info: MethodType =>
      val paramInfo :: Nil = info.paramInfos: @unchecked
      val newParamInfo =
        CapturingType(paramInfo, CaptureSet.universal)
      val trackedParam = info.paramRefs.head
      def newResult(tp: Type): Type = tp match
        case tp: MethodOrPoly =>
          tp.derivedLambdaType(resType = newResult(tp.resType))
        case _ =>
          CapturingType(tp, CaptureSet(trackedParam))
      info.derivedLambdaType(paramInfos = newParamInfo :: Nil, resType = newResult(info.resType))
        .showing(i"augment unapply type $info to $result", capt)
    case info: PolyType =>
      info.derivedLambdaType(resType = addUnapplyCaptures(info.resType))

  /** Drop added capture information from the type of an `unapply` */
  private def dropUnapplyCaptures(info: Type)(using Context): Type = info match
    case info: MethodType =>
      info.paramInfos match
        case CapturingType(oldParamInfo, _) :: Nil =>
          def oldResult(tp: Type): Type = tp match
            case tp: MethodOrPoly =>
              tp.derivedLambdaType(resType = oldResult(tp.resType))
            case CapturingType(tp, _) =>
              tp
          info.derivedLambdaType(paramInfos = oldParamInfo :: Nil, resType = oldResult(info.resType))
        case _ =>
          info
    case info: PolyType =>
      info.derivedLambdaType(resType = dropUnapplyCaptures(info.resType))

  /** If `sym` refers to a synthetic apply, unapply, copy, or copy default getter method
   *  of a case class, transform it to account for capture information.
   *  The method is run in phase CheckCaptures.Pre
   *  @pre needsTransform(sym)
   */
  def transformToCC(sym: SymDenotation)(using Context): SymDenotation = sym.name match
    case DefaultGetterName(nme.copy, n) =>
      sym.copySymDenotation(info = addDefaultGetterCapture(sym.info, sym.owner, n))
    case nme.unapply =>
      sym.copySymDenotation(info = addUnapplyCaptures(sym.info))
    case nme.apply | nme.copy =>
      sym.copySymDenotation(info = addCaptureDeps(sym.info))
    case n if n == nme.eq || n == nme.ne =>
      sym.copySymDenotation(info =
        MethodType(defn.ObjectType.capturing(CaptureSet.universal) :: Nil, defn.BooleanType))

  /** If `sym` refers to a synthetic apply, unapply, copy, or copy default getter method
   *  of a case class, transform it back to what it was before the CC phase.
   *  @pre needsTransform(sym)
   */
  def transformFromCC(sym: SymDenotation)(using Context): SymDenotation = sym.name match
    case DefaultGetterName(nme.copy, n) =>
      sym.copySymDenotation(info = dropDefaultGetterCapture(sym.info))
    case nme.unapply =>
      sym.copySymDenotation(info = dropUnapplyCaptures(sym.info))
    case nme.apply | nme.copy =>
      sym.copySymDenotation(info = dropCaptureDeps(sym.info))
    case n if n == nme.eq || n == nme.ne =>
      sym.copySymDenotation(info = defn.methOfAnyRef(defn.BooleanType))

end Synthetics