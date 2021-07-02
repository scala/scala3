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
          capt.println(i"typing closure ${tree1.meth.symbol} with fvs ${capturedVars(tree1.meth.symbol)}")
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

  def checkWellFormed(whole: Type, pos: SrcPos)(using Context): Unit =
    def checkRelativeVariance(mt: MethodType) = new TypeTraverser:
      def traverse(tp: Type): Unit = tp match
        case CapturingType(parent, ref) =>
          ref.stripTypeVar match
            case ref @ TermParamRef(`mt`, _) if variance <= 0 =>
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


object CheckCaptures:
  import ast.tpd.*

  def expandFunctionTypes(using Context) =
    ctx.settings.Ycc.value && !ctx.settings.YccNoAbbrev.value && !ctx.isAfterTyper

  object FunctionTypeTree:
    def unapply(tree: Tree)(using Context): Option[(List[Type], Type)] =
      if defn.isFunctionType(tree.tpe) then
        tree match
          case AppliedTypeTree(tycon: TypeTree, args) =>
            Some((args.init.tpes, args.last.tpe))
          case RefinedTypeTree(_, (appDef: DefDef) :: Nil) if appDef.span == tree.span =>
            appDef.symbol.info match
              case mt: MethodType => Some((mt.paramInfos, mt.resultType))
              case _ => None
          case _ =>
            None
      else None

  object CapturingTypeTree:
    def unapply(tree: Tree)(using Context): Option[(Tree, Tree, CaptureRef)] = tree match
      case AppliedTypeTree(tycon, parent :: _ :: Nil)
      if tycon.symbol == defn.Predef_retainsType =>
        tree.tpe match
          case CapturingType(_, ref) => Some((tycon, parent, ref))
          case _ => None
      case _ => None

  def addRetains(tree: Tree, ref: CaptureRef)(using Context): Tree =
    untpd.AppliedTypeTree(
        TypeTree(defn.Predef_retainsType.typeRef), List(tree, TypeTree(ref)))
      .withType(CapturingType(tree.tpe, ref))
      .showing(i"add inferred capturing $result", capt)

  /**  Under -Ycc but not -Ycc-no-abbrev, if `tree` represents a function type
   *   `(ARGS) => T` where `T` has capture set CS1, expand it to
   *   `(ARGS) => T retains CS2` where CS2 consists of those elements in CS1
   *   that are not accounted for by the capture set of any argument in ARGS.
   *   The additions will be removed again if the function type is wrapped in an
   *   explicit `retains` type.
   */
  def addResultCaptures(tree: Tree)(using Context): Tree =
    if expandFunctionTypes then
      tree match
        case FunctionTypeTree(argTypes, resType) =>
          val cs = resType.captureSet
          (tree /: cs.elems)((t, ref) =>
            if argTypes.exists(_.captureSet.accountsFor(ref)) then t
            else addRetains(t, ref)
          )
        case _ =>
          tree
    else tree

  private def addCaptures(tp: Type, refs: Type)(using Context): Type = refs match
    case ref: CaptureRef => CapturingType(tp, ref)
    case OrType(refs1, refs2) => addCaptures(addCaptures(tp, refs1), refs2)
    case _ => tp

  /** @pre: `tree is a tree of the form `T retains REFS`.
   *  Return the same tree with `parent1` instead of `T` with its type
   *  recomputed accordingly.
   */
  private def derivedCapturingTree(tree: AppliedTypeTree, parent1: Tree)(using Context): AppliedTypeTree =
    tree match
      case AppliedTypeTree(tycon, parent :: (rest @ (refs :: Nil))) if parent ne parent1 =>
        cpy.AppliedTypeTree(tree)(tycon, parent1 :: rest)
          .withType(addCaptures(parent1.tpe, refs.tpe))
      case _ =>
        tree

  private def stripCaptures(tree: Tree, ref: CaptureRef)(using Context): Tree = tree match
    case tree @ AppliedTypeTree(tycon, parent :: refs :: Nil) if tycon.symbol == defn.Predef_retainsType =>
      val parent1 = stripCaptures(parent, ref)
      val isSynthetic = tycon.isInstanceOf[TypeTree]
      if isSynthetic then
        parent1.showing(i"drop inferred capturing $tree => $result", capt)
      else
        if parent1.tpe.captureSet.accountsFor(ref) then
          report.warning(
            em"redundant capture: $parent1 already contains $ref with capture set ${ref.captureSet} in its capture set ${parent1.tpe.captureSet}",
            tree.srcPos)
        derivedCapturingTree(tree, parent1)
    case _ => tree

  private def stripCaptures(tree: Tree, refs: Type)(using Context): Tree = refs match
    case ref: CaptureRef => stripCaptures(tree, ref)
    case OrType(refs1, refs2) => stripCaptures(stripCaptures(tree, refs1), refs2)
    case _ => tree

  /** If this is a tree of the form `T retains REFS`,
   *   - strip any synthesized captures directly in T;
   *   - warn if a reference in REFS is accounted for by the capture set of the remaining type
   */
  def refineNestedCaptures(tree: AppliedTypeTree)(using Context): AppliedTypeTree = tree match
    case AppliedTypeTree(tycon, parent :: (rest @ (refs :: Nil))) if tycon.symbol == defn.Predef_retainsType =>
      derivedCapturingTree(tree, stripCaptures(parent, refs.tpe))
    case _ =>
      tree

end CheckCaptures

