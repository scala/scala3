package dotty.tools
package dotc
package typer

import core._
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types._
import Symbols._
import StdNames._
import Decorators._
import ProtoTypes._
import Inferencing.isFullyDefined
import config.Printers.refinr
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

/** A class that can be used to do type checking again after the first typer phase
 *  is run. This phase will use the output of the previous typer but "forget" certain things
 *  so that they can be reinferred. Things that are forgotten fall into the following
 *  categories:
 *
 *   1. Bindings of inferred type variables in type applications.
 *   2. Inferred types of local or private vals or vars. Exception: Types of
 *      inline vals and Java-defined fields are kept.
 *   3. Inferred result types of local or private methods. Eception: Types
 *      of default getters and Java-defined methods are kept.
 *      (The default getter restriction is there for technical reason, we should be
 *      able to lift it once we change the scheme for default arguments).
 *   4. Types of closure parameters that are inferred from the expected type.
 *      Types of closure parameters that are inferred from the called method
 *      are left alone (also for technical reasons).
 *
 *  The re-typed trees and associated symbol infos are thrown away once the phase
 *  has ended. So the phase can be only used for more refined type checking, but
 *  not for code transformation.
 */
class CheckCaptures extends RefineTypes:
  import ast.tpd.*

  def phaseName: String = "cc"
  override def isEnabled(using Context) = ctx.settings.Ycc.value

  def newRefiner() = CaptureChecker()

  class CaptureChecker extends TypeRefiner:
    import ast.tpd.*
    override def newLikeThis: Typer = CaptureChecker()

    private var myDeps: Dependencies = null

    def deps(using Context): Dependencies =
      if myDeps == null then
        myDeps = new Dependencies(ctx.compilationUnit.tpdTree, ctx):
          def isExpr(sym: Symbol)(using Context): Boolean =
            sym.isRealClass || sym.isOneOf(MethodOrLazy)
          def enclosure(using Context) =
            def recur(owner: Symbol): Symbol =
              if isExpr(owner) || !owner.exists then owner else recur(owner.owner)
            recur(ctx.owner)
      myDeps

    private def capturedVars(sym: Symbol)(using Context): CaptureSet =
      CaptureSet(deps.freeVars(sym).toList.map(_.termRef).filter(_.isTracked)*)

    override def typedClosure(tree: untpd.Closure, pt: Type)(using Context): Tree =
      super.typedClosure(tree, pt) match
        case tree1: Closure =>
          refinr.println(i"typing closure ${tree1.meth.symbol} with fvs ${capturedVars(tree1.meth.symbol)}")
          tree1.withType(tree1.tpe.capturing(capturedVars(tree1.meth.symbol)))
        case tree1 => tree1

    override def typedApply(tree: untpd.Apply, pt: Type)(using Context): Tree =
      super.typedApply(tree, pt) match
        case tree1 @ Apply(fn, args) =>
          if tree.fun.symbol.isConstructor then
            //println(i"typing $tree1, ${capturedVars(tree1.tpe.classSymbol)}")
            tree1.withType(tree1.tpe.capturing(capturedVars(tree1.tpe.classSymbol)))
          else
            tree1
        case tree1 => tree1

  end CaptureChecker

  inline val disallowGlobal = true

  def checkRelativeVariance(mt: MethodType, whole: Type, pos: SrcPos)(using Context) = new TypeTraverser:
    def traverse(tp: Type): Unit = tp match
      case CapturingType(parent, ref @ TermParamRef(`mt`, _)) =>
        if variance <= 0 then
          val direction = if variance < 0 then "contra" else "in"
          report.error(em"captured reference $ref appears ${direction}variantly in type $whole", pos)
        traverse(parent)
      case _ =>
        traverseChildren(tp)

  def checkWellFormed(whole: Type, pos: SrcPos)(using Context): Unit =
    val checkVariance = new TypeTraverser:
      def traverse(tp: Type): Unit = tp match
        case mt: MethodType if mt.isResultDependent =>
          checkRelativeVariance(mt, whole, pos).traverse(mt)
        case _ =>
          traverseChildren(tp)
    checkVariance.traverse(whole)

  object PostRefinerCheck extends TreeTraverser:
    def traverse(tree: Tree)(using Context) =
      tree match
        case tree1 @ TypeApply(fn, args) =>
          for arg <- args do
            //println(i"checking $arg in $tree: ${arg.tpe.captureSet}")
            for ref <- arg.tpe.captureSet.elems do
              val isGlobal = ref match
                case ref: TypeRef => ref.isRootCapability
                case ref: TermRef => ref.prefix != NoPrefix && ref.symbol.hasAnnotation(defn.AbilityAnnot)
              val what = if ref.isRootCapability then "universal" else "global"
              if isGlobal then
                val notAllowed = i" is not allowed to capture the $what capability $ref"
                def msg = arg match
                  case arg: InferredTypeTree =>
                    i"""inferred type argument ${arg.tpe}$notAllowed
                       |
                       |The inferred arguments are: [$args%, %]"""
                  case _ => s"type argument$notAllowed"
                report.error(msg, arg.srcPos)
        case tree: TypeTree =>
          // it's inferred, no need to check
        case tree: TypTree =>
          checkWellFormed(tree.tpe, tree.srcPos)
        case tree: DefDef =>
          def check(tp: Type): Unit = tp match
            case tp: PolyType =>
              check(tp.resType)
            case mt: MethodType =>
              if mt.isResultDependent then
                checkRelativeVariance(mt, tree.symbol.info, ctx.source.atSpan(tree.nameSpan)).traverse(mt)
              check(mt.resType)
            case _ =>
          check(tree.symbol.info)
        case _ =>
      traverseChildren(tree)

  def postRefinerCheck(tree: tpd.Tree)(using Context): Unit =
    if disallowGlobal then PostRefinerCheck.traverse(tree)

end CheckCaptures
