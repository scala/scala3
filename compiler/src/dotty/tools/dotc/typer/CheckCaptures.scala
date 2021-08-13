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

class CheckCaptures extends Recheck:
  import ast.tpd.*

  def phaseName: String = "cc"
  override def isEnabled(using Context) = ctx.settings.Ycc.value

  def newRechecker()(using Context) = CaptureChecker(ctx)

  class CaptureChecker(ictx: Context) extends Rechecker(ictx):
    import ast.tpd.*

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

/*

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

*/

  end CaptureChecker

  inline val disallowGlobal = true

  // ^^^ todo: drop wf condition; work into parameter substitution
  def checkWellFormed(whole: Type, pos: SrcPos)(using Context): Unit =
    def checkRelativeVariance(mt: MethodType) = new TypeTraverser:
      def traverse(tp: Type): Unit = tp match
        case CapturingType(parent, refs) =>
          for ref <- refs.elems do
            ref match
              case TermParamRef(`mt`, _) =>
                if variance <= 0 then
                  val direction = if variance < 0 then "contra" else "in"
                  report.error(em"captured reference $ref appears ${direction}variantly in type $whole", pos)
              case _ =>
          traverse(parent)
        case _ =>
          traverseChildren(tp)
    val checkVariance = new TypeTraverser:
      def traverse(tp: Type): Unit = tp match
        case mt: MethodType if mt.isResultDependent =>
          checkRelativeVariance(mt).traverse(mt)
        case _ =>
          traverseChildren(tp)
    checkVariance.traverse(whole)

  object PostRefinerCheck extends TreeTraverser:
    def traverse(tree: Tree)(using Context) =
      tree match
        case tree1 @ TypeApply(fn, args) if disallowGlobal =>
          for arg <- args do
            //println(i"checking $arg in $tree: ${arg.tpe.captureSet}")
            for ref <- arg.tpe.captureSet.elems do
              val isGlobal = ref match
                case ref: TypeRef => ref.isRootCapability
                case ref: TermRef => ref.prefix != NoPrefix && ref.symbol.hasAnnotation(defn.AbilityAnnot)
                case _ => false
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
        case _: TypTree | _: Closure =>
          checkWellFormed(tree.tpe, tree.srcPos)
        case tree: DefDef =>
          def check(tp: Type): Unit = tp match
            case tp: MethodOrPoly => check(tp.resType)
            case _ =>
          check(tree.symbol.info)
        case _ =>
      traverseChildren(tree)

  def postRefinerCheck(tree: tpd.Tree)(using Context): Unit =
    PostRefinerCheck.traverse(tree)

end CheckCaptures
