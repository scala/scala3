package dotty.tools
package dotc
package typer

import core._
import SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types._
import Symbols._
import StdNames._
import Decorators._
import ProtoTypes._
import Inferencing.isFullyDefined
import config.Printers.capt
import ast.{tpd, untpd, Trees}
import NameKinds.{DocArtifactName, OuterSelectName, DefaultGetterName}
import Trees._
import scala.util.control.NonFatal
import typer.ErrorReporting._
import util.Spans.Span
import util.{SimpleIdentitySet, SrcPos}
import util.Chars.*
import Nullables._
import transform.*
import scala.collection.mutable
import reporting._
import ProtoTypes._
import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions

/** 1. Copy outer retains sets to functions on the right
 *  2. Add tracked parameters to the capture sets of functions on the right
 *  3. Copy captures of result to functions on the left unless accounted for or universally quantified.
 */
object ExpandCaptures:
  import ast.tpd.*

  /** An extractor for all kinds of function type trees: simple, dependent, or polymoprhic.
   */
  object FunctionType:

    /** Recover (1) type and (2) value parameters and (3) result of function type.
     *  Three cases:
     *    Simple functions: type parameters are empty, value parameters are general types
     *    Dependent functions: type parameters are empty, value patameters are ParamRefs
     *    Polymorphic fucntions: type parameters are nonempty, value parameters are ParamRefs
     */
    def unapply(tpe: Type)(using Context): Option[(List[TypeParamRef], List[Type], Type)] = tpe match
      case RefinedType(parent, nme.apply, rinfo: MethodType)
      if defn.isNonRefinedFunction(parent) =>
        Some((Nil, rinfo.paramRefs, rinfo.resType))
      case RefinedType(parent, nme.apply, rinfo: PolyType)
      if parent.isRef(defn.PolyFunctionClass) =>
        rinfo.resType match
          case mt: MethodType => Some((rinfo.paramRefs, mt.paramRefs, mt.resType))
          case _ => Some((rinfo.paramRefs, Nil, rinfo.resType))
      case defn.FunctionOf(argTypes, resType, _, _) =>
        Some((Nil, argTypes, resType))
      case _ =>
        None

    /** A copy of `tpe` (which must be a FunctionType) with a new `result` */
    def copy(tpe: Type)(result: Type)(using Context): Type = tpe match
      case rt @ RefinedType(parent, app, rinfo: MethodType) =>
        rt.derivedRefinedType(parent, app, rinfo.derivedLambdaType(resType = result))
      case rt @ RefinedType(parent, app, rinfo: PolyType) =>
        rt.derivedRefinedType(parent, app,
          rinfo.derivedLambdaType(
            resType = rinfo.resType match
              case mt: MethodType => mt.derivedLambdaType(resType = result)
              case _ => result
          )
        )
      case defn.FunctionOf(argTypes, resType, isContextual, isErased) =>
        if resType ne result then defn.FunctionOf(argTypes, result, isContextual, isErased)
        else tpe

    /** Convert non-dependent FunctionType to equivalent refined type */
    def toDependent(tpe: Type)(using Context) = tpe match
      case defn.FunctionOf(argTypes, resType, isContextual, isErased) =>
        val methodType = MethodType.companion(isContextual = isContextual, isErased = isErased)
        val rinfo = methodType(argTypes, resType)
        RefinedType(tpe, nme.apply, rinfo)

  end FunctionType

  /** Add implied capture sets to function type trees
   *  @param bound         the set of bound type variables enclosing the current tree
   *  @param outerCaptures the implied captures coming from the outside
   *  @param canAdd        whether implied captures can be added to the next enclosed function tree.
   *                       This is false if the function tree is directly enclosed in an explicit retains node.
   */
  private def addImplied(tpe: Type, bound: CaptureSet, outerCaptures: CaptureSet, canAdd: Boolean, pos: SrcPos)(using Context): Type =
  trace(i"addImplied $tpe, bound = $bound, outer = $outerCaptures, can add = $canAdd", capt, show = true) {

    def paramCaptures(args: List[Type]): CaptureSet =
      args.foldLeft(CaptureSet.empty)((cs, arg) => cs ++ arg.captureSet)

    def nestedCaptures(tpe: Type): CaptureSet = tpe match
      case CapturingType(parent, _) =>
        nestedCaptures(parent)
      case FunctionType(tparams, params, body) =>
        body.captureSet -- paramCaptures(tparams) -- paramCaptures(params)
      case _ =>
        CaptureSet.empty

    def wrapImplied(tpe: Type) =
      if canAdd then
        (tpe /: (outerCaptures ++ nestedCaptures(tpe)).elems)(CapturingType(_, _))
      else tpe

    def reportOverlap(declared: CaptureSet, implied: CaptureSet, direction: String): Unit =
      val redundant = declared.intersect(implied)
      if redundant.nonEmpty then
        report.echo(
          i"declared captures $redundant in $tpe are redundant since they are already implied by the capture sets coming from the $direction",
          pos)

    tpe match
      case tpe @ CapturingType(parent, ref) =>
        reportOverlap(ref.singletonCaptureSet, outerCaptures, "left")
        reportOverlap(ref.singletonCaptureSet, nestedCaptures(parent), "right")
        val parent1 = addImplied(parent, bound, outerCaptures + ref, canAdd = false, pos)
        tpe.derivedCapturingType(parent1, ref)
      case FunctionType(tparams, params, body) =>
        val newParamCaptures = paramCaptures(params) -- bound -- outerCaptures
        if newParamCaptures.nonEmpty && !tpe.isInstanceOf[RefinedType] then
          val refined = FunctionType.toDependent(tpe)
          val refined1 = addImplied(refined, bound, outerCaptures, canAdd, pos)
          if refined1 ne refined then refined1 else tpe
        else
          wrapImplied(
            FunctionType.copy(tpe)(
              addImplied(
                body,
                bound ++ paramCaptures(tparams),
                outerCaptures ++ newParamCaptures,
                canAdd = true, pos)))
      case _ =>
        tpe
  }
  end addImplied

  def apply(tpe: Type, pos: SrcPos)(using Context): Type =
    addImplied(tpe, CaptureSet.empty, CaptureSet.empty, canAdd = true, pos)

end ExpandCaptures

