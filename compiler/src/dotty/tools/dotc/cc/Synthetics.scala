package dotty.tools
package dotc
package cc

import core.*
import Symbols.*, SymDenotations.*, Contexts.*, Flags.*, Types.*, Decorators.*
import StdNames.nme
import Names.Name
import NameKinds.DefaultGetterName
import config.Printers.capt
import Capabilities.*

/** Classification and transformation methods for function methods and
 *  synthetic case class methods that need to be treated specially.
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

  private val functionCombinatorNames = Set[Name](
    nme.andThen, nme.compose, nme.curried, nme.tupled)

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
    || defn.isFunctionClass(symd.owner) && functionCombinatorNames.contains(symd.name)

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

  /** Transform the type of a method either to its type under capture checking
   *  or back to its previous type.
   *  @param  symd The method to transform @pre needsTransform(sym) must hold.
   *  @param  info The possibly already mapped info of sym
   */
  def transform(symd: SymDenotation, info: Type)(using Context): SymDenotation =

    /** Add capture dependencies to the type of the `apply` or `copy` method of a case class.
     *  An apply method in a case class like this:
     *    case class CC(a: A^{d}, b: B, c: C^{any})
     *  would get type
     *    def apply(a': A^{d}, b: B, c': C^{any}): CC^{a', c'} { val a = A^{a'}, val c = C^{c'} }
     *  where `'` is used to indicate the difference between parameter symbol and refinement name.
     *  Analogous for the copy method.
     */
    def addCaptureDeps(info: Type): Type = info match
      case info: MethodType =>
        val trackedParams = info.paramRefs.filter(atPhase(Phases.checkCapturesPhase)(_.isTracked))
        def augmentResult(tp: Type): Type = tp match
          case tp: MethodOrPoly =>
            tp.derivedLambdaType(resType = augmentResult(tp.resType))
          case _ =>
            val refined = trackedParams.foldLeft(tp): (parent, pref) =>
              parent.refinedOverride(pref.paramName,
                CapturingType(
                  atPhase(ctx.phase.next)(pref.underlying.stripCapturing),
                  CaptureSet(pref)))
            CapturingType(refined, CaptureSet(trackedParams*))
        if trackedParams.isEmpty then info
        else augmentResult(info).showing(i"augment apply/copy type $info to $result", capt)
      case info: PolyType =>
        info.derivedLambdaType(resType = addCaptureDeps(info.resType))
      case _ =>
        info

    /** Add capture information to the type of the default getter of a case class copy method
     */
    def transformDefaultGetterCaptures(info: Type, owner: Symbol, idx: Int)(using Context): Type = info match
      case info: MethodOrPoly =>
        info.derivedLambdaType(resType = transformDefaultGetterCaptures(info.resType, owner, idx))
      case info: ExprType =>
        info.derivedExprType(transformDefaultGetterCaptures(info.resType, owner, idx))
      case CapturingType(parent, _) =>
        transformDefaultGetterCaptures(parent, owner, idx)
      case info @ AnnotatedType(parent, annot) =>
        info.derivedAnnotatedType(transformDefaultGetterCaptures(parent, owner, idx), annot)
      case _ if idx < owner.asClass.paramGetters.length =>
        val param = owner.asClass.paramGetters(idx)
        val pinfo = param.info
        atPhase(ctx.phase.next) {
          if pinfo.captureSet.isAlwaysEmpty then info
          else CapturingType(pinfo.stripCapturing, CaptureSet(param.termRef))
        }
      case _ =>
        info

    /** Augment an unapply of type `(x: C): D` to `(x: C^{any}): D^{x}` */
    def transformUnapplyCaptures(info: Type)(using Context): Type = info match
      case info: MethodType =>
        val paramInfo :: Nil = info.paramInfos: @unchecked
        val newParamInfo = CapturingType(paramInfo, CaptureSet.universal)
        val trackedParam = info.paramRefs.head
        def newResult(tp: Type): Type = tp match
          case tp: MethodOrPoly =>
            tp.derivedLambdaType(resType = newResult(tp.resType))
          case _ =>
            CapturingType(tp, CaptureSet(trackedParam))
        info.derivedLambdaType(paramInfos = newParamInfo :: Nil, resType = newResult(info.resType))
          .showing(i"augment unapply type $info to $result", capt)
      case info: PolyType =>
        info.derivedLambdaType(resType = transformUnapplyCaptures(info.resType))

    def transformComposeCaptures(info: Type, owner: Symbol) =
      val (pt: PolyType) = info: @unchecked
      val (mt: MethodType) = pt.resType: @unchecked
      val (enclThis: ThisType) = owner.thisType: @unchecked
      val paramCaptures = CaptureSet(enclThis, GlobalAny)
      pt.derivedLambdaType(resType = MethodType(mt.paramNames)(
        mt1 => mt.paramInfos.map(_.capturing(paramCaptures)),
        mt1 => CapturingType(mt.resType, CaptureSet(enclThis, mt1.paramRefs.head))))

    def transformCurriedTupledCaptures(info: Type, owner: Symbol) =
      val (et: ExprType) = info: @unchecked
      val (enclThis: ThisType) = owner.thisType: @unchecked
      def mapFinalResult(tp: Type, f: Type => Type): Type = tp match
        case FunctionOrMethod(args, res) =>
          tp.derivedFunctionOrMethod(args, mapFinalResult(res, f))
        case _ =>
          f(tp)
      ExprType(mapFinalResult(et.resType, CapturingType(_, CaptureSet(enclThis))))

    def transformCompareCaptures =
      val (enclThis: ThisType) = symd.owner.thisType: @unchecked
      MethodType(
        defn.ObjectType.capturing(CaptureSet(GlobalAny, enclThis)) :: Nil,
        defn.BooleanType)

    symd.copySymDenotation(info = symd.name match
      case DefaultGetterName(nme.copy, n) =>
        transformDefaultGetterCaptures(info, symd.owner, n)
      case nme.unapply =>
        transformUnapplyCaptures(info)
      case nme.apply | nme.copy =>
        addCaptureDeps(info)
      case nme.andThen | nme.compose =>
        transformComposeCaptures(info, symd.owner)
      case nme.curried | nme.tupled =>
        transformCurriedTupledCaptures(info, symd.owner)
      case n if n == nme.eq || n == nme.ne =>
        transformCompareCaptures)
  end transform

end Synthetics
